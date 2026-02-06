import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { Dependency, PeriodicTaskService, ServiceStatus } from '@rosen-bridge/service-manager';
import { OutputBox, Request } from '@ergo-raffle/box-lookup';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { configs } from '../../config/config';
import { BoxLookupService } from '../boxLookupService';
import { DBService } from '../dbService';
import { ScannerService } from '../scannerService';
import { TxPotService } from '../txPotService';
import { loadRosenContracts } from '../../config/contractsConfig';
import { mnemonicToAddress, signTx, getNextCleanupFromTx, getNextRepoFromTx, getCollateralFromTx } from '../../utils/ergoUtils';
import { CleanupTxType } from '../../types'
import { createCollateralRequest } from '../../utils/boxLookupUtils';
import {
  outputBoxToErgoBox,
  getCommitmentCountFromR7,
  getWidListDigestFromR4,
  findCollateralBoxByWid,
  hasToken,
  getTokenAmount,
  getWidFromR4Bytes,
} from '../../utils/cleanupUtils';
import { FraudTx, TriggerEventData } from '@rosen-bridge/fraud-tx';
import { SlashTx } from '@rosen-bridge/slash-tx';
import { RosenContracts } from '../../types'
import { ERGO_CHAIN_NAME } from '../../config/constants';
import { TransactionStatus } from '@rosen-bridge/tx-pot';
import { CleanupWorkflowState } from './state';
import { registerCleanupRequests } from './requests';

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
  private readonly state = new CleanupWorkflowState();

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
    BoxLookupService.getInstance().removeRequest(this.state.requestIds.trigger);
    BoxLookupService.getInstance().removeRequest(this.state.requestIds.fraud);
    BoxLookupService.getInstance().removeRequest(this.state.requestIds.cleanup);
    BoxLookupService.getInstance().removeRequest(this.state.requestIds.repo);

    for (const requestId of this.state.pendingCollateralRequestsByWid.values()) {
      BoxLookupService.getInstance().removeRequest(requestId);
    }
    this.state.resetAll();
  };

  /**
   * End-of-round hook called after each `BoxLookup.serveRequests()` run.
   * Clears cached boxes so the next round refills them from box-lookup.
   *
   * @returns void
   */
  private onBoxLookupRoundEnd = async (): Promise<void> => {
    this.state.resetRound();
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
      this.state.requestIds.trigger !== undefined ||
      this.state.requestIds.fraud !== undefined ||
      this.state.requestIds.cleanup !== undefined ||
      this.state.requestIds.repo !== undefined
    ) {
      return;
    }

    this.state.requestIds = registerCleanupRequests({
      contracts,
      cleanupAddress,
      onCleanupSuffice: this.onCleanupSuffice,
      onRepoSuffice: this.onRepoSuffice,
      onTriggerEventSuffice: this.onTriggerEventSuffice,
      onFraudBoxSuffice: this.onFraudBoxSuffice,
    });
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
    height: number
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
      height
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
    this.state.cleanupCache = { cleanupBox, feeBoxes };
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

    this.state.repoBoxCache = candidate;
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
      if (!this.state.cleanupCache) {
        this.logger.warn('cleanup cache is not initialized in onTriggerEventSuffice');
        return;
      };

      if (trigger.creationHeight > height - this.contracts.cleanupConfirm) {
        this.logger.info(`skipping fraud tx build for trigger [${trigger.boxId}]: creationHeight (${trigger.creationHeight}) > height (${height}) - cleanupConfirm (${this.contracts.cleanupConfirm})`);
        continue;
      };
      const { cleanupBox, feeBoxes } = this.state.cleanupCache;

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
      );
      this.state.cleanupCache = getNextCleanupFromTx(signed, this.contracts.tokens.CleanupNFT);
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
      const wid = getWidFromR4Bytes(outputBoxToErgoBox(fraud));
      const q = this.state.fraudQueueByWid.get(wid) ?? [];
      q.push(fraud);
      this.state.fraudQueueByWid.set(wid, q);
    }

    // Register at most one collateral request per wid in this round.
    for (const [wid, q] of this.state.fraudQueueByWid.entries()) {
      if (this.state.pendingCollateralRequestsByWid.has(wid)) {
        continue;
      }
      const requestId = this.registerCollateralRequest(wid, contracts, cleanupAddress);
      this.state.pendingCollateralRequestsByWid.set(wid, requestId);
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

      if (!this.state.cleanupCache || !this.state.repoBoxCache) {
        this.logger.warn('cleanup cache or repo box cache is not initialized in onCollateralSuffice');
        return;
      }

      // Initialize starting collateral for this wid in this round (chain/mempool aware).
      let currentCollateral = this.state.collateralBoxByWid.get(wid) ?? collateralBoxes[0];
      this.state.collateralBoxByWid.set(wid, currentCollateral);

      const q = this.state.fraudQueueByWid.get(wid) ?? [];

      while (q.length > 0) {
        const nextFraud = q.shift()!;
        const height = await ScannerService.getInstance().getCurrentHeight();
        const { cleanupBox, feeBoxes } = this.state.cleanupCache;
        const repoBox = this.state.repoBoxCache;
        const collateralBox = this.state.collateralBoxByWid.get(wid)!;

        SlashTx.init(configs.workflow.minBoxValue, configs.workflow.txFee, this.logger);

        const result = await SlashTx.getInstance()
          .newBuilder()
          .setFraudBox(outputBoxToErgoBox(nextFraud))
          .setCollateralBox(outputBoxToErgoBox(collateralBox))
          .setRepoBox(outputBoxToErgoBox(repoBox))
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
        );
        this.state.cleanupCache = getNextCleanupFromTx(signedTx, contracts.tokens.CleanupNFT);
        this.state.repoBoxCache = getNextRepoFromTx(signedTx, contracts.tokens.RepoNFT);
        this.state.collateralBoxByWid.set(wid, getCollateralFromTx(signedTx, contracts.tokens.AwcNFT));
      }

    } finally {
      const requestId = this.state.pendingCollateralRequestsByWid.get(wid);
      if (requestId) {
        BoxLookupService.getInstance().removeRequest(requestId);
        this.state.pendingCollateralRequestsByWid.delete(wid);
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

  /**
   * Periodic tick used to keep the service active.
   */
  private workflowTick = async (): Promise<void> => {
    // No-op: all work is triggered by box-lookup callbacks.
  };
}



