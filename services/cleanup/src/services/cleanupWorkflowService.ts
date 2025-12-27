import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { AbstractService, Dependency, ServiceStatus } from '@rosen-bridge/service-manager';
import { TransactionStatus, TxOptions, TxPot } from '@rosen-bridge/tx-pot';
import { OutputBox, Request } from '@ergo-raffle/box-lookup';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { configs } from '../config';
import { BoxLookupService } from './boxLookupService';
import { DBService } from './dbService';
import { ScannerService } from './scannerService';
import { TxPotService } from './txPotService';
import { loadRosenContracts } from './ergo/contractsConfig';
import { mnemonicToAddress } from './ergo/keys';
import { signTx } from './ergo/signTx';
import { getNextCleanupFromTx, getNextRepoFromTx } from './ergo/txOutputs';
import { CleanupTxType } from '../types/cleanupTxType';
import {
  createCollateralRequest,
  createCleanupRequest,
  createFraudBoxRequest,
  createRepoRequest,
  createTriggerEventRequest,
} from './boxLookup';
import {
  outputBoxToErgoBox,
  getCommitmentCountFromR7,
  getWidListDigestFromR4,
  findCollateralBoxByWid,
  hasToken,
  toCollateralBoxData,
  toRwtRepoData,
  getTokenAmount,
  getWidFromR4Bytes,
} from '../utils/cleanup';
import { FraudTx, TriggerEventData } from '@rosen-bridge/fraud-tx';
import { SlashTx } from '@rosen-bridge/slash-tx';
import { RosenContracts } from '../types/contracts';
import { ERGO_CHAIN_NAME } from '../constants';
import { FraudBoxData } from '@rosen-bridge/slash-tx';

export class CleanupWorkflowService extends AbstractService {
  static name = 'CleanupWorkflowService';
  name = CleanupWorkflowService.name;
  private static instance?: CleanupWorkflowService;

  protected dependencies: Dependency[] = [
    { serviceName: DBService.name, allowedStatuses: [ServiceStatus.running] },
    { serviceName: ScannerService.name, allowedStatuses: [ServiceStatus.running] },
    { serviceName: TxPotService.name, allowedStatuses: [ServiceStatus.running] },
    { serviceName: BoxLookupService.name, allowedStatuses: [ServiceStatus.running] },
  ];

  private isJobRunning = false;
  private scheduledJob?: NodeJS.Timeout;
  private shouldStopJob = false;
  private continueStop: () => void = () => undefined;

  private contracts?: RosenContracts;
  private txPot: TxPot = TxPotService.getInstance().getTxPot();
  private cleanupAddress?: string;
  private triggerRequestId?: number;
  private fraudRequestId?: number;
  private cleanupRequestId?: number;
  private repoRequestId?: number;

  private cleanupCache?: { cleanupBox: ergoLib.ErgoBox; feeBoxes: ergoLib.ErgoBox[] };
  private repoBoxCache?: OutputBox;
  private pendingCollateralRequests = new Map<string, number>();
  private collateralTree?: string;

  private constructor(logger?: AbstractLogger) {
    super(logger);
  }

  /**
   * Initializes the singleton instance.
   *
   * @param logger - Optional logger
   */
  static init = (logger?: AbstractLogger) => {
    if (this.instance) return;
    this.instance = new CleanupWorkflowService(logger);
  };

  /**
   * Returns the singleton instance.
   *
   * @returns CleanupWorkflowService instance
   */
  static getInstance = (): CleanupWorkflowService => {
    if (!this.instance) throw new Error('CleanupWorkflowService instance is not initialized yet');
    return this.instance;
  };

  /**
   * Starts the periodic workflow loop.
   *
   * @returns True when started
   */
  protected start = async (): Promise<boolean> => {
    this.prepareRuntime();
    this.registerRequests();
    BoxLookupService.getInstance().onAfterServe(this.onBoxLookupRoundEnd);
    this.job();
    this.setStatus(ServiceStatus.running);
    return true;
  };

  /**
   * Stops the periodic workflow loop.
   *
   * @returns True when stopped
   */
  protected stop = async (): Promise<boolean> => {
    if (this.isJobRunning) {
      await new Promise<void>((resolve) => {
        this.continueStop = resolve;
        this.shouldStopJob = true;
      });
    }
    BoxLookupService.getInstance().removeRequest(this.triggerRequestId);
    this.triggerRequestId = undefined;

    BoxLookupService.getInstance().removeRequest(this.fraudRequestId);
    this.fraudRequestId = undefined;

    BoxLookupService.getInstance().removeRequest(this.cleanupRequestId);
    this.cleanupRequestId = undefined;

    BoxLookupService.getInstance().removeRequest(this.repoRequestId);
    this.repoRequestId = undefined;

    for (const requestId of this.pendingCollateralRequests.values()) {
      BoxLookupService.getInstance().removeRequest(requestId);
    }
    this.pendingCollateralRequests.clear();
    clearTimeout(this.scheduledJob);
    this.shouldStopJob = false;
    this.setStatus(ServiceStatus.dormant);
    return true;
  };

  /**
   * Registers box-lookup requests needed for the cleanup workflow.
   */
  private registerRequests = () => {
    if (!this.contracts || !this.cleanupAddress) {
      throw new Error('CleanupWorkflowService is not prepared');
    }
    const contracts = this.contracts;
    const cleanupAddress = this.cleanupAddress;

    if (
      this.triggerRequestId !== undefined ||
      this.fraudRequestId !== undefined ||
      this.cleanupRequestId !== undefined ||
      this.repoRequestId !== undefined
    ) {
      return;
    }

    // TODO frad package should provide a "minNeededValue"
    this.cleanupRequestId = BoxLookupService.getInstance().addRequest(
      createCleanupRequest(
        ergoLib.Address.from_base58(cleanupAddress).to_ergo_tree().to_base16_bytes(),
        BigInt(configs.workflow.txFee) + BigInt(configs.workflow.minBoxValue),
        [{ tokenId: contracts.tokens.CleanupNFT, amount: 1n }],
        async () => ScannerService.getInstance().getUnspentBoxesByAddress(cleanupAddress),
        this.onCleanupSuffice,
      ),
    );

    this.repoRequestId = BoxLookupService.getInstance().addRequest(
      createRepoRequest(
        ergoLib.Address.from_base58(contracts.addresses.RWTRepo).to_ergo_tree().to_base16_bytes(),
        undefined,
        [{ tokenId: contracts.tokens.RepoNFT, amount: 1n }],
        async () => ScannerService.getInstance().getUnspentBoxesByAddress(contracts.addresses.RWTRepo),
        this.onRepoSuffice,
      ),
    );

    this.triggerRequestId = BoxLookupService.getInstance().addRequest(
      createTriggerEventRequest(
        ergoLib.Address.from_base58(contracts.addresses.WatcherTriggerEvent)
          .to_ergo_tree()
          .to_base16_bytes(),
        undefined,
        [{ tokenId: contracts.tokens.RWTId, amount: 1n }],
        async () => {
          const height = await ScannerService.getInstance().getCurrentHeight();
          const expiredBefore = height - contracts.cleanupConfirm;
          const confirmed = await ScannerService.getInstance().getUnspentTriggerBoxes();
          return confirmed.filter((b) => b.creationHeight <= expiredBefore);
        },
        this.onTriggerEventSuffice,
      ),
    );

    this.fraudRequestId = BoxLookupService.getInstance().addRequest(
      createFraudBoxRequest(
        ergoLib.Address.from_base58(contracts.addresses.Fraud).to_ergo_tree().to_base16_bytes(),
        undefined,
        [{ tokenId: contracts.tokens.RWTId, amount: 1n }],
        async () => ScannerService.getInstance().getUnspentFraudBoxes(),
        this.onFraudBoxSuffice,
      ),
    );
  };

  /**
   * Box-lookup callback for cleanup address: caches cleanup box and fee boxes.
   */
  private onCleanupSuffice: Request['onSuffice'] = async (boxes) => {
    if (!this.contracts) {
      throw new Error('CleanupWorkflowService is not prepared');
    }
    const contracts = this.contracts;
    let cleanupBox: ergoLib.ErgoBox | undefined;
    const feeBoxes: ergoLib.ErgoBox[] = [];
    for (const b of boxes) {
      const parsed = outputBoxToErgoBox(b);
      if (!cleanupBox && hasToken(parsed, contracts.tokens.CleanupNFT)) {
        cleanupBox = parsed;
      } else {
        feeBoxes.push(parsed);
      }
    }
    if (!cleanupBox) return;
    this.cleanupCache = { cleanupBox, feeBoxes };
  };

  /**
   * Box-lookup callback for repo address: caches the repo box.
   */
  private onRepoSuffice: Request['onSuffice'] = async (boxes) => {
    if (!this.contracts) {
      throw new Error('CleanupWorkflowService is not prepared');
    }
    const contracts = this.contracts;
    const candidate = boxes.find((b) => hasToken(outputBoxToErgoBox(b), contracts.tokens.RepoNFT));
    if (!candidate) return;

    this.repoBoxCache = candidate;
  };

  /**
   * Signs an unsigned tx and adds it to TxPot in SIGNED state.
   *
   * @param txType - Cleanup tx type
   * @param unsignedTx - Unsigned tx
   * @param inputBoxes - Input boxes used for signing
   * @param height - Current chain height (used for tx-pot lastCheck)
   * @param extra - Work id (boxId) for idempotency
   * @param extra2 - Secondary key (usually source tx id)
   */
  private signAndEnqueueTx = async (
    txType: CleanupTxType,
    unsignedTx: ergoLib.UnsignedTransaction,
    inputBoxes: ergoLib.ErgoBox[],
    height: number,
    extra: string,
    extra2: string,
  ): Promise<void> => {
    if (!this.contracts) {
      throw new Error('CleanupWorkflowService is not prepared');
    }
    const signed = await signTx(
      configs.scanner.nodeUrl,
      configs.workflow.cleanupMnemonic,
      unsignedTx,
      inputBoxes,
    );
    const serializedTx = Buffer.from(signed.sigma_serialize_bytes()).toString('base64');
    await this.txPot.addTx(
      signed.id().to_str(),
      ERGO_CHAIN_NAME,
      txType,
      0,
      serializedTx,
      TransactionStatus.SIGNED,
      height,
      extra,
      extra2,
    );

    const nextCleanup = getNextCleanupFromTx(signed, this.contracts.tokens.CleanupNFT);
    this.cleanupCache = nextCleanup;

    if (txType === CleanupTxType.slash) {
      this.repoBoxCache = getNextRepoFromTx(signed, this.contracts.tokens.RepoNFT);
    }
  };

  /**
   * End-of-round hook called after each `BoxLookup.serveRequests()` run.
   * Clears cached boxes so the next round refills them from box-lookup.
   *
   * @returns void
   */
  private onBoxLookupRoundEnd = async (): Promise<void> => {
    this.cleanupCache = undefined;
    this.repoBoxCache = undefined;
  };

  /**
   * Box-lookup callback for trigger-event boxes: builds and enqueues a fraud tx.
   */
  private onTriggerEventSuffice: Request['onSuffice'] = async (boxes) => {
    if (!this.contracts || !this.cleanupAddress) {
      throw new Error('CleanupWorkflowService is not prepared');
    }
    const height = await ScannerService.getInstance().getCurrentHeight();

    for (const trigger of boxes) {
      if (!this.cleanupCache) return;
      if (trigger.creationHeight > height - this.contracts.cleanupConfirm) continue;
      const { cleanupBox, feeBoxes } = this.cleanupCache;

      // TODO the tx may be invalid and we may need to retry it, if isEnqueued ignores invalid txs then we are fine
      if (await TxPotService.getInstance().isEnqueued(CleanupTxType.fraud, trigger.boxId)) continue;

      const triggerBox = outputBoxToErgoBox(trigger);
      const digest = getWidListDigestFromR4(triggerBox);
      const commitmentCount = getCommitmentCountFromR7(triggerBox);

      const wids = await ScannerService.getInstance().getTriggerWidsByTxId(trigger.transactionId);
      if (wids.length !== commitmentCount) {
        this.logger.warn(
          `Skipping fraud tx build for trigger [${trigger.boxId}]: commitmentCount mismatch for triggerTxId=${trigger.transactionId}. ` +
            `commitmentCount=${commitmentCount}, wids.length=${wids.length}, widDigest=${digest}`,
        );
        continue;
      }

      const triggerData: TriggerEventData = {
        box: triggerBox,
        wids,
        rwtAmount: getTokenAmount(triggerBox, this.contracts.tokens.RWTId),
      };

      FraudTx.init(
        this.contracts.addresses.Fraud,
        this.cleanupAddress,
        this.contracts.tokens.RWTId,
        configs.workflow.minBoxValue,
        configs.workflow.txFee,
        this.logger,
      );
      const result = await FraudTx.getInstance()
        .newBuilder()
        .setTriggerEventData(triggerData)
        .setCleanerBox(cleanupBox)
        .setCreationHeight(height)
        .setFeeBoxes(feeBoxes)
        .setChangeAddress(this.cleanupAddress)
        .build();
      await this.signAndEnqueueTx(
        CleanupTxType.fraud,
        result.unsignedTx,
        result.inputBoxes,
        height,
        trigger.boxId,
        trigger.transactionId,
      );
    }
  };

  /**
   * Box-lookup callback for fraud boxes: builds and enqueues a slash tx.
   */
  private onFraudBoxSuffice: Request['onSuffice'] = async (boxes: OutputBox[]) => {
    if (!this.contracts || !this.cleanupAddress) {
      throw new Error('CleanupWorkflowService is not prepared');
    }
    const contracts = this.contracts;
    const cleanupAddress = this.cleanupAddress;

    for (const fraud of boxes) {
      // TODO same as trigger event box, the tx may be invalid and we may need to retry it, if isEnqueued ignores invalid txs then we are fine
      if (await TxPotService.getInstance().isEnqueued(CleanupTxType.slash, fraud.boxId)) continue;
      const fraudBox = outputBoxToErgoBox(fraud);

      const fraudData: FraudBoxData = {
        box: fraudBox,
        wid: getWidFromR4Bytes(fraudBox),
        rwtAmount: getTokenAmount(fraudBox, contracts.tokens.RWTId),
      }

      // De-duplicate: create one collateral request per fraud box id.
      if (this.pendingCollateralRequests.has(fraud.boxId)) continue;
      const requestId = this.registerCollateralRequest(fraud, fraudData, contracts, cleanupAddress);
      this.pendingCollateralRequests.set(fraud.boxId, requestId);
    }
  };

  /**
   * Registers a box-lookup request to fetch the collateral box needed for slashing a fraud box.
   *
   * @param fraud - Fraud box
   * @param fraudData - Parsed fraud data (includes WID)
   * @param contracts - Rosen contracts config
   * @param cleanupAddress - Cleanup address for change outputs
   * @returns Registered request id
   */
  private registerCollateralRequest = (
    fraud: OutputBox,
    fraudData: FraudBoxData,
    contracts: RosenContracts,
    cleanupAddress: string,
  ): number => {
    if (!this.collateralTree) {
      throw new Error('CleanupWorkflowService is not prepared');
    }
    return BoxLookupService.getInstance().addRequest(
      createCollateralRequest(
        this.collateralTree,
        undefined,
        // TODO understand AwcNFT and collateral structure
        [{ tokenId: contracts.tokens.AwcNFT, amount: 1n }],
        async (): Promise<OutputBox[]> => {
          const confirmed = await ScannerService.getInstance().getUnspentCollateralBoxes();
          try {
            const match = findCollateralBoxByWid(confirmed, contracts.tokens.AwcNFT, fraudData.wid);
            return [match];
          } catch {
            return [];
          }
        },
        async (boxes: OutputBox[]) => await this.onCollateralSuffice(fraud, fraudData, contracts, cleanupAddress, boxes),

      ),
    );
  };

  /**
   * Box-lookup callback for a dynamically registered collateral request.
   * Builds and enqueues a slash transaction, then unregisters the request.
   *
   * @param fraud - Fraud box associated with this request
   * @param fraudData - Parsed fraud data (includes WID)
   * @param contracts - Rosen contracts config
   * @param cleanupAddress - Cleanup address for change outputs
   * @param boxes - Selected boxes for the request (collateral candidates)
   */
  private onCollateralSuffice = async (
    fraud: OutputBox,
    fraudData: FraudBoxData,
    contracts: RosenContracts,
    cleanupAddress: string,
    boxes: OutputBox[],
  ): Promise<void> => {
    try {
      if (await TxPotService.getInstance().isEnqueued(CleanupTxType.slash, fraud.boxId)) return;
      if (!this.cleanupCache || !this.repoBoxCache) return;

      const height = await ScannerService.getInstance().getCurrentHeight();
      const { cleanupBox, feeBoxes } = this.cleanupCache;
      const repoBox = this.repoBoxCache;

      const collateralBox = findCollateralBoxByWid(boxes, contracts.tokens.AwcNFT, fraudData.wid);

      SlashTx.init(
        contracts.addresses.RWTRepo,
        contracts.addresses.WatcherCollateral,
        cleanupAddress,
        configs.workflow.minBoxValue,
        configs.workflow.txFee,
        this.logger,
      );
      const result = await SlashTx.getInstance()
        .newBuilder()
        .setFraudBoxData(fraudData)
        .setCollateralBoxData(toCollateralBoxData(collateralBox, contracts.tokens.RSN))
        .setRepoData(
          toRwtRepoData(repoBox, {
            repoNftTokenId: contracts.tokens.RepoNFT,
            rwtTokenId: contracts.tokens.RWTId,
            rsnTokenId: contracts.tokens.RSN,
          }),
        )
        .setCleanupBox(cleanupBox)
        .setCreationHeight(height)
        .setFeeBoxes(feeBoxes)
        .setChangeAddress(cleanupAddress)
        .build();
      await this.signAndEnqueueTx(
        CleanupTxType.slash,
        result.unsignedTx,
        result.inputBoxes,
        height,
        fraud.boxId,
        fraud.transactionId,
      );
    } finally {
      const requestId = this.pendingCollateralRequests.get(fraud.boxId);
      if (requestId) {
        BoxLookupService.getInstance().removeRequest(requestId);
        this.pendingCollateralRequests.delete(fraud.boxId);
      }
    }
  };

  /**
   * Initializes runtime dependencies from config.
   * Loads contract config and derives cleanup address.
   */
  private prepareRuntime = () => {
    this.contracts = loadRosenContracts(configs.workflow.contractsPath);

    const prefix =
      configs.workflow.ergoNetwork === 'mainnet'
        ? ergoLib.NetworkPrefix.Mainnet
        : ergoLib.NetworkPrefix.Testnet;
    this.cleanupAddress = mnemonicToAddress(configs.workflow.cleanupMnemonic, prefix);
    this.collateralTree = ergoLib.Address.from_base58(this.contracts.addresses.WatcherCollateral)
      .to_ergo_tree()
      .to_base16_bytes();
  };

  /**
   * Periodic job that will coordinate cleanup work items.
   */
  private job = async (): Promise<void> => {
    this.isJobRunning = true;
    try {
    } finally {
      this.scheduledJob = setTimeout(this.job, configs.intervals.workflow * 1000);
      this.isJobRunning = false;
      if (this.shouldStopJob) {
        this.shouldStopJob = false;
        this.continueStop();
      }
    }
  };
}


