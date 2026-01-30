import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { Dependency, PeriodicTaskService, ServiceStatus } from '@rosen-bridge/service-manager';
import { OutputBox, Request } from '@ergo-raffle/box-lookup';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { configs } from '../config/config';
import { BoxLookupService } from './boxLookupService';
import { DBService } from './dbService';
import { ScannerService } from './scannerService';
import { TxPotService } from './txPotService/txPotService';
import { loadRosenContracts } from '../config/contractsConfig';
import { mnemonicToAddress, signTx, getNextCleanupFromTx, getNextRepoFromTx, getCollateralFromTx } from '../utils/ergoUtils';
import { CleanupTxType } from '../types'
import {
  createCollateralRequest,
  createCleanupRequest,
  createFraudBoxRequest,
  createRepoRequest,
  createTriggerEventRequest,
} from '../utils/boxLookupUtils';
import {
  outputBoxToErgoBox,
  getCommitmentCountFromR7,
  getWidListDigestFromR4,
  findCollateralBoxByWid,
  hasToken,
  toRwtRepoData,
  getTokenAmount,
  getWidFromR4Bytes,
} from '../utils/cleanupUtils';
import { FraudTx, TriggerEventData } from '@rosen-bridge/fraud-tx';
import { SlashTx } from '@rosen-bridge/slash-tx';
import { RosenContracts } from '../types'
import { ERGO_CHAIN_NAME } from '../config/constants';
import { TransactionStatus } from '@rosen-bridge/tx-pot';

export class CleanupWorkflowService extends PeriodicTaskService {
  static name = 'CleanupWorkflowService';
  protected name = CleanupWorkflowService.name;
  taskName = 'CleanupWorkflowTask';
  private static instance?: CleanupWorkflowService;

  protected dependencies: Dependency[] = [
    { serviceName: DBService.name, allowedStatuses: [ServiceStatus.running] },
    { serviceName: ScannerService.name, allowedStatuses: [ServiceStatus.running] },
    { serviceName: TxPotService.name, allowedStatuses: [ServiceStatus.running] },
    { serviceName: BoxLookupService.name, allowedStatuses: [ServiceStatus.running] },
  ];

  private contracts?: RosenContracts;
  private cleanupAddress?: string;
  private triggerRequestId?: number;
  private fraudRequestId?: number;
  private cleanupRequestId?: number;
  private repoRequestId?: number;

  private cleanupCache?: { cleanupBox: ergoLib.ErgoBox; feeBoxes: ergoLib.ErgoBox[] };
  private repoBoxCache?: OutputBox;
  // wid -> requestId (per-round de-dupe: only one collateral fetch request per watcher)
  private pendingCollateralRequestsByWid = new Map<string, number>();
  // wid -> frauds to be slashed in this round
  private fraudQueueByWid = new Map<string, OutputBox[]>();
  // wid -> latest collateral box to use within this round
  private collateralBoxByWid = new Map<string, OutputBox>();

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
   * Initializes runtime requirements before periodic tasks start.
   */
  protected preStart = async (): Promise<void> => {
    this.prepareRuntime();
    this.registerRequests();
    BoxLookupService.getInstance().onAfterServe(this.onBoxLookupRoundEnd);
  };

  /**
   * Cleans up requests after periodic tasks stop.
   */
  protected postStop = async (): Promise<void> => {
    BoxLookupService.getInstance().onAfterServe(async () => undefined);
    BoxLookupService.getInstance().removeRequest(this.triggerRequestId);
    this.triggerRequestId = undefined;

    BoxLookupService.getInstance().removeRequest(this.fraudRequestId);
    this.fraudRequestId = undefined;

    BoxLookupService.getInstance().removeRequest(this.cleanupRequestId);
    this.cleanupRequestId = undefined;

    BoxLookupService.getInstance().removeRequest(this.repoRequestId);
    this.repoRequestId = undefined;

    for (const requestId of this.pendingCollateralRequestsByWid.values()) {
      BoxLookupService.getInstance().removeRequest(requestId);
    }
    this.pendingCollateralRequestsByWid.clear();
    this.fraudQueueByWid.clear();
    this.collateralBoxByWid.clear();
    this.cleanupCache = undefined;
    this.repoBoxCache = undefined;
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
    this.pendingCollateralRequestsByWid.clear();
    this.fraudQueueByWid.clear();
    this.collateralBoxByWid.clear();
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

    this.cleanupRequestId = BoxLookupService.getInstance().addRequest(
      createCleanupRequest(
        ergoLib.Address.from_base58(cleanupAddress).to_ergo_tree().to_base16_bytes(),
        BigInt(configs.workflow.txFee) + BigInt(configs.workflow.minCleanupValue),  // could be improved
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
  ): Promise<ergoLib.Transaction> => {
    if (!this.contracts) {
      throw new Error('CleanupWorkflowService is not prepared');
    }
    const ctx = await TxPotService.getInstance().getErgoNetworkInterface().network.getErgoStateContext();
    const signed = await signTx(
      ctx,
      configs.workflow.cleanupMnemonic,
      unsignedTx,
      inputBoxes
    );
    const serializedTx = Buffer.from(signed.sigma_serialize_bytes()).toString('base64');
    this.logger.info(`signing and enqueuing tx ${signed.id().to_str()} for ${txType} with ${inputBoxes.length} input boxes`);
    await TxPotService.getInstance().getTxPot().addTx(
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

    return signed;
  };


  /**
   * Box-lookup callback for cleanup address: caches cleanup box and fee boxes.
   */
  private onCleanupSuffice: Request['onSuffice'] = async (boxes) => {
    if (!this.contracts) {
      throw new Error('CleanupWorkflowService is not prepared');
    }
    this.logger.info(`onCleanupSuffice: got ${boxes.length} cleanup boxes`);

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
  private onRepoSuffice: Request['onSuffice'] = async (boxes: OutputBox[]) => {
    if (!this.contracts) {
      throw new Error('CleanupWorkflowService is not prepared');
    }
    this.logger.info(`onRepoSuffice: got ${boxes.length} repo boxes`);

    const contracts = this.contracts;
    const candidate = boxes.find((b: OutputBox) =>
      hasToken(outputBoxToErgoBox(b), contracts.tokens.RepoNFT),
    );
    if (!candidate) return;

    this.repoBoxCache = candidate;
  };

  /**
   * Box-lookup callback for trigger-event boxes: builds and enqueues a fraud tx.
   */
  private onTriggerEventSuffice: Request['onSuffice'] = async (boxes: OutputBox[]) => {
    if (!this.contracts || !this.cleanupAddress) {
      throw new Error('CleanupWorkflowService is not prepared');
    }
    this.logger.info(`onTriggerEventSuffice: got ${boxes.length} trigger-event boxes`);

    const height = await ScannerService.getInstance().getCurrentHeight();

    for (const trigger of boxes) {
      if (!this.cleanupCache) {
        this.logger.warn('cleanup cache is not initialized in onTriggerEventSuffice');
        return;
      };

      if (trigger.creationHeight > height - this.contracts.cleanupConfirm) {
        this.logger.info(`skipping fraud tx build for trigger [${trigger.boxId}]: creationHeight (${trigger.creationHeight}) > height (${height}) - cleanupConfirm (${this.contracts.cleanupConfirm})`);
        continue;
      };
      const { cleanupBox, feeBoxes } = this.cleanupCache;


      const isAlreadyEnqueued = await TxPotService.getInstance().isEnqueued(CleanupTxType.fraud, trigger.boxId);
      if (isAlreadyEnqueued) {
        this.logger.info(`skipping fraud tx build for trigger [${trigger.boxId}]: already enqueued`);
        continue;
      }

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
      
      
      const signed = await this.signAndEnqueueTx(

        CleanupTxType.fraud,
        result.unsignedTx,
        result.inputBoxes,
        height,
        trigger.boxId,
        trigger.transactionId,
      );
      this.cleanupCache = getNextCleanupFromTx(signed, this.contracts.tokens.CleanupNFT);
    }
  };

  /**
   * Box-lookup callback for fraud boxes: builds and enqueues a slash tx.
   */
  private onFraudBoxSuffice: Request['onSuffice'] = async (boxes: OutputBox[]) => {
    if (!this.contracts || !this.cleanupAddress) {
      throw new Error('CleanupWorkflowService is not prepared');
    }
    this.logger.info(`onFraudBoxSuffice: got ${boxes.length} fraud boxes`);

    const contracts = this.contracts;
    const cleanupAddress = this.cleanupAddress;

    // Queue all new frauds by wid (some wids may have multiple fraud boxes that must be slashed sequentially).
    for (const fraud of boxes) {
      const isAlreadyEnqueued = await TxPotService.getInstance().isEnqueued(
        CleanupTxType.slash,
        fraud.boxId,
      );
      if (isAlreadyEnqueued) {
        this.logger.info(
          `skipping fraud tx build for fraud box [${fraud.boxId}]: already enqueued`,
        );
        continue;
      }

      const wid = getWidFromR4Bytes(outputBoxToErgoBox(fraud));
      const q = this.fraudQueueByWid.get(wid) ?? [];
      q.push(fraud);
      this.fraudQueueByWid.set(wid, q);
    }

    // Register at most one collateral request per wid in this round.
    for (const [wid, q] of this.fraudQueueByWid.entries()) {
      if (this.pendingCollateralRequestsByWid.has(wid)) {
        continue;
      }
      const requestId = this.registerCollateralRequest(wid, contracts, cleanupAddress);
      this.pendingCollateralRequestsByWid.set(wid, requestId);
    }
  };

  /**
   * Registers a box-lookup request to fetch the collateral box needed for slashing a fraud box.
   *
   * @param fraud - Fraud box
   * @param fraudBox - Parsed fraud box
   * @param contracts - Rosen contracts config
   * @param cleanupAddress - Cleanup address for change outputs
   * @returns Registered request id
   */
  private registerCollateralRequest = (
    wid: string,
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
        [{ tokenId: contracts.tokens.AwcNFT, amount: 1n }],
        async (): Promise<OutputBox[]> => {
          const confirmed = await ScannerService.getInstance().getUnspentCollateralBoxes();
          try {
            return findCollateralBoxByWid(confirmed, contracts.tokens.AwcNFT, wid);
          } catch {
            return [];
          }
        },
        async (boxes: OutputBox[]) => await this.onCollateralSuffice(wid, contracts, cleanupAddress, boxes),

      ),
    );
  };

  /**
   * Box-lookup callback for a dynamically registered collateral request.
   * Builds and enqueues a slash transaction, then unregisters the request.
   *
   * @param fraud - Fraud box associated with this request
   * @param fraudBox - Parsed fraud box
   * @param wid - Watcher ID from fraud box
   * @param contracts - Rosen contracts config
   * @param cleanupAddress - Cleanup address for change outputs
   * @param boxes - Selected boxes for the request (collateral candidates)
   */
  private onCollateralSuffice = async (
    wid: string,
    contracts: RosenContracts,
    cleanupAddress: string,
    boxes: OutputBox[],
  ): Promise<void> => {
    const collateralBoxes = findCollateralBoxByWid(boxes, contracts.tokens.AwcNFT, wid);
    if (collateralBoxes.length === 0) {
      this.logger.info(`got ${boxes.length} collateral boxes but none is for this fraud box, skipping.`);
      return;
    }
    try {

      if (!this.cleanupCache || !this.repoBoxCache) {
        this.logger.warn('cleanup cache or repo box cache is not initialized in onCollateralSuffice');
        return;
      }

      // Initialize starting collateral for this wid in this round (chain/mempool aware).
      let currentCollateral = this.collateralBoxByWid.get(wid) ?? collateralBoxes[0];
      this.collateralBoxByWid.set(wid, currentCollateral);

      const q = this.fraudQueueByWid.get(wid) ?? [];

      while (q.length > 0) {
        const nextFraud = q.shift()!;
        // if a "valid" tx with the same boxId is already enqueued, skip it
        if (await TxPotService.getInstance().isEnqueued(CleanupTxType.slash, nextFraud.boxId)) {
          this.logger.info(`skipping fraud tx build for fraud box [${nextFraud.boxId}]: already enqueued`);
          continue;
        }
        const height = await ScannerService.getInstance().getCurrentHeight();
        const { cleanupBox, feeBoxes } = this.cleanupCache;
        const repoBox = this.repoBoxCache;
        const collateralBox = this.collateralBoxByWid.get(wid)!;

        SlashTx.init(configs.workflow.minBoxValue, configs.workflow.txFee, this.logger);

        const result = await SlashTx.getInstance()
          .newBuilder()
          .setFraudBox(outputBoxToErgoBox(nextFraud))
          .setCollateralBox(outputBoxToErgoBox(collateralBox))
          .setRepoData(
            toRwtRepoData(repoBox, {
              repoNftTokenId: contracts.tokens.RepoNFT,
              rwtTokenId: contracts.tokens.RWTId,
              rsnTokenId: contracts.tokens.RSN,
              awcTokenId: contracts.tokens.AwcNFT,
            }),
          )
          .setCleanupBox(cleanupBox)
          .setCreationHeight(height)
          .setFeeBoxes(feeBoxes)
          .setChangeAddress(cleanupAddress)
          .build();

        const signedTx = await this.signAndEnqueueTx(
          CleanupTxType.slash,
          result.unsignedTx,
          result.inputBoxes,
          height,
          nextFraud.boxId,
          nextFraud.transactionId,
        );
        this.cleanupCache = getNextCleanupFromTx(signedTx, contracts.tokens.CleanupNFT);
        this.repoBoxCache = getNextRepoFromTx(signedTx, contracts.tokens.RepoNFT);
        this.collateralBoxByWid.set(wid, getCollateralFromTx(signedTx, contracts.tokens.AwcNFT));
      }

    } finally {
      const requestId = this.pendingCollateralRequestsByWid.get(wid);
      if (requestId) {
        BoxLookupService.getInstance().removeRequest(requestId);
        this.pendingCollateralRequestsByWid.delete(wid);
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
   * Periodic job that keeps the workflow alive.
   */
  protected getTasks = () => {
    return [
      {
        fn: this.workflowTick,
        interval: configs.intervals.workflow * 1000,
      },
    ];
  };

  private workflowTick = async (): Promise<void> => {
    // No-op: all work is triggered by box-lookup callbacks.
  };
}



