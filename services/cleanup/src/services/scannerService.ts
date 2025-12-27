import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { BlockEntity } from '@rosen-bridge/abstract-scanner';
import { BoxEntity, ErgoUTXOExtractor } from '@rosen-bridge/address-extractor';
import { ErgoNodeNetwork, ErgoScanner } from '@rosen-bridge/ergo-scanner';
import { FraudExtractor, FraudEntity } from '@rosen-bridge/fraud-extractor';
import { AbstractService, Dependency, ServiceStatus } from '@rosen-bridge/service-manager';
import { ErgoNetworkType } from '@rosen-bridge/scanner-interfaces';
import {
  CollateralEntity,
  CollateralExtractor,
  CommitmentEntity,
  CommitmentExtractor,
  EventTriggerEntity,
  EventTriggerExtractor,
} from '@rosen-bridge/watcher-data-extractor';
import { OutputBox } from '@ergo-raffle/box-lookup';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { TokenMap } from '@rosen-bridge/tokens';
import { IsNull } from '@rosen-bridge/extended-typeorm';

import { DBService } from './dbService';
import { RosenContracts } from '../types/contracts';
import { serializedErgoBoxToOutputBox } from '../utils/scanner';

export class ScannerService extends AbstractService {
  static name = 'ScannerService';
  name = ScannerService.name;
  private static instance?: ScannerService;
  private ergoScanner: ErgoScanner;
  private extractorsRegistered = false;
  private readonly nodeUrl: string;
  private isJobRunning = false;
  private scheduledJob?: NodeJS.Timeout;
  private shouldStopJob = false;
  private continueStop: () => void = () => undefined;
  private readonly updateInterval: number;

  protected dependencies: Dependency[] = [
    { serviceName: DBService.name, allowedStatuses: [ServiceStatus.running] },
  ];

  private constructor(
    updateInterval: number,
    nodeUrl: string,
    private readonly explorerUrl: string,
    initialHeight: number,
    private readonly networkPrefix: ergoLib.NetworkPrefix,
    private readonly contracts: RosenContracts,
    private readonly trackedAddresses: string[],
    logger?: AbstractLogger,
  ) {
    super(logger);
    this.updateInterval = updateInterval;
    this.nodeUrl = nodeUrl;
    this.ergoScanner = new ErgoScanner({
      network: new ErgoNodeNetwork(nodeUrl),
      initialHeight,
      dataSource: DBService.getInstance().dataSource,
      logger: this.logger,
    });
  }

  /**
   * Initializes the singleton instance.
   *
   * @param updateInterval - Interval in seconds
   * @param nodeUrl - Ergo node URL
   * @param explorerUrl - Ergo explorer URL (used by some existing extractors)
   * @param initialHeight - Scanner starting height
   * @param networkPrefix - Ergo network prefix (mainnet/testnet)
   * @param contracts - Contract config (addresses + tokens for extractors)
   * @param trackedAddresses - Addresses to track via address-extractor (e.g., cleanup address, repo)
   * @param logger - Optional logger
   */
  static init = (
    updateInterval: number,
    nodeUrl: string,
    explorerUrl: string,
    initialHeight: number,
    networkPrefix: ergoLib.NetworkPrefix,
    contracts: RosenContracts,
    trackedAddresses: string[],
    logger?: AbstractLogger,
  ) => {
    if (this.instance) return;
    this.instance = new ScannerService(
      updateInterval,
      nodeUrl,
      explorerUrl,
      initialHeight,
      networkPrefix,
      contracts,
      trackedAddresses,
      logger,
    );
  };

  /**
   * Returns the singleton instance.
   *
   * @returns ScannerService instance
   */
  static getInstance = (): ScannerService => {
    if (!this.instance) throw new Error('ScannerService instance is not initialized yet');
    return this.instance;
  };

  /**
   * Starts the periodic scanner update loop.
   *
   * @returns True when started
   */
  protected start = async (): Promise<boolean> => {
    this.registerExtractorsIfNeeded();
    this.job();
    this.setStatus(ServiceStatus.running);
    return true;
  };

  /**
   * Stops the periodic scanner update loop.
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
    clearTimeout(this.scheduledJob);
    this.shouldStopJob = false;
    this.setStatus(ServiceStatus.dormant);
    return true;
  };

  /**
   * Returns confirmed (mined) unspent boxes for an address from the scanner DB.
   * These are produced by `@rosen-bridge/address-extractor` and do not require
   * direct network calls in cleanup-service.
   *
   * @param address - Base58 address
   * @returns Unspent boxes as `OutputBox[]`
   */
  getUnspentBoxesByAddress = async (address: string): Promise<OutputBox[]> => {
    const repo = DBService.getInstance().dataSource.getRepository(BoxEntity);
    const rows = await repo.find({
      where: { address, spendBlock: IsNull() },
    });
    return rows.map((row) => serializedErgoBoxToOutputBox(row.serialized));
  };

  /**
   * Returns confirmed (mined) unspent trigger boxes extracted by `EventTriggerExtractor`.
   *
   * @returns Trigger boxes as `OutputBox[]`
   */
  getUnspentTriggerBoxes = async (): Promise<OutputBox[]> => {
    const repo = DBService.getInstance().dataSource.getRepository(EventTriggerEntity);
    const rows = await repo.find({
      where: { spendBlock: IsNull() },
    });
    return rows.map((row) => serializedErgoBoxToOutputBox(row.serialized));
  };

  /**
   * Returns confirmed (mined) unspent fraud boxes extracted by `FraudExtractor`.
   *
   * @returns Fraud boxes as `OutputBox[]`
   */
  getUnspentFraudBoxes = async (): Promise<OutputBox[]> => {
    const repo = DBService.getInstance().dataSource.getRepository(FraudEntity);
    const rows = await repo.find({
      where: { spendBlock: IsNull() },
    });
    return rows.map((row) => serializedErgoBoxToOutputBox(row.serialized));
  };

  /**
   * Returns confirmed (mined) unspent collateral boxes extracted by `CollateralExtractor`.
   *
   * @returns Collateral boxes as `OutputBox[]`
   */
  getUnspentCollateralBoxes = async (): Promise<OutputBox[]> => {
    const repo = DBService.getInstance().dataSource.getRepository(CollateralEntity);
    const rows = await repo.find({
      where: { spendBlock: IsNull() },
    });
    return rows.map((row) => serializedErgoBoxToOutputBox(row.boxSerialized));
  };

  /**
   * Returns watcher WIDs for a trigger tx by reading commitments spent by that tx.
   * Result order follows tx input ordering via `spendIndex`.
   *
   * @param triggerTxId - Trigger transaction id
   * @returns Ordered list of WIDs (hex)
   */
  getTriggerWidsByTxId = async (triggerTxId: string): Promise<string[]> => {
    const repo = DBService.getInstance().dataSource.getRepository(CommitmentEntity);
    const rows = await repo.find({
      where: { spendTxId: triggerTxId },
      order: { spendIndex: 'ASC' },
    });
    return rows
      .filter((row) => row.spendIndex !== null && row.spendIndex !== undefined)
      .map((row) => row.WID);
  };

  /**
   * Returns the latest processed block height in the scanner DB.
   *
   * @returns Latest processed height, or 0 when scanner has not persisted any blocks
   */
  getCurrentHeight = async (): Promise<number> => {
    const repo = DBService.getInstance().dataSource.getRepository(BlockEntity);
    const row = await repo.findOne({
      order: { height: 'DESC' },
    });
    return row?.height ?? 0;
  };

  /**
   * Registers address extractors for all configured addresses (once).
   */
  private registerExtractorsIfNeeded = () => {
    if (this.extractorsRegistered) return;
    const extractorDataSource = DBService.getInstance().dataSource;

    this.trackedAddresses.forEach((address) => {
      const extractor = new ErgoUTXOExtractor(
        extractorDataSource,
        `utxo-${address}`,
        this.networkPrefix,
        this.nodeUrl,
        ErgoNetworkType.Node,
        address,
        undefined,
        this.logger,
        true,
      );
      this.ergoScanner.registerExtractor(extractor);
    });

    this.ergoScanner.registerExtractor(
      new EventTriggerExtractor(
        'event-trigger-extractor',
        extractorDataSource,
        ErgoNetworkType.Explorer,
        this.explorerUrl,
        this.contracts.addresses.WatcherTriggerEvent,
        this.contracts.tokens.RWTId,
        this.contracts.addresses.WatcherPermit,
        this.contracts.addresses.Fraud,
        this.logger,
        true,
      ),
    );
    this.ergoScanner.registerExtractor(
      new FraudExtractor(
        extractorDataSource,
        'fraud-extractor',
        this.explorerUrl,
        this.contracts.addresses.Fraud,
        this.contracts.tokens.RWTId,
        this.logger,
      ),
    );
    this.ergoScanner.registerExtractor(
      new CollateralExtractor(
        'collateral-extractor',
        this.contracts.tokens.AwcNFT,
        this.contracts.addresses.WatcherCollateral,
        extractorDataSource,
        this.explorerUrl,
        this.logger,
      ),
    );

    this.ergoScanner.registerExtractor(
      new CommitmentExtractor(
        'commitment-extractor',
        [this.contracts.addresses.Commitment],
        this.contracts.tokens.RWTId,
        extractorDataSource,
        new TokenMap(this.logger),
        this.logger,
      ),
    );

    this.extractorsRegistered = true;
  };

  /**
   * Periodic job that runs `scanner.update()`.
   */
  private job = async (): Promise<void> => {
    this.isJobRunning = true;
    try {
      await this.ergoScanner.update();
    } catch (e) {
      this.logger.warn(`Scanner update failed: ${e}`);
      if (e instanceof Error && e.stack) this.logger.warn(e.stack);
    } finally {
      this.isJobRunning = false;
    }

    if (this.shouldStopJob) {
      this.shouldStopJob = false;
      this.continueStop();
      return;
    }

    this.scheduledJob = setTimeout(this.job, this.updateInterval * 1000);
  };
}


