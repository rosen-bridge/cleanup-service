import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { AbstractService, Dependency, ServiceStatus } from '@rosen-bridge/service-manager';
import { TxOptions, TxPot } from '@rosen-bridge/tx-pot';
import { DataSource } from '@rosen-bridge/extended-typeorm';
import { DBService } from './dbService';
import { ERGO_CHAIN_NAME } from '../constants';
import { ErgoNetworkInterface } from '../txPot/ergoNetworkInterface';
import { CleanupTxType } from '../types/cleanupTxType';

export class TxPotService extends AbstractService {
  static name = 'TxPotService';
  name = TxPotService.name;
  private static instance?: TxPotService;

  protected dependencies: Dependency[] = [
    { serviceName: DBService.name, allowedStatuses: [ServiceStatus.running] },
  ];

  private isJobRunning = false;
  private scheduledJob?: NodeJS.Timeout;
  private shouldStopJob = false;
  private continueStop: () => void = () => undefined;

  private constructor(
    private updateInterval: number,
    private dataSource: DataSource,
    private txRequiredConfirmations: number,
    logger?: AbstractLogger,
  ) {
    super(logger);
  }

  /**
   * Initializes the singleton instance.
   *
   * @param updateInterval - Interval in seconds
   * @param dataSource - TypeORM DataSource (tx-pot storage)
   * @param txRequiredConfirmations - Required confirmations for tx validity
   * @param logger - Optional logger
   */
  static init = (
    updateInterval: number,
    dataSource: DataSource,
    txRequiredConfirmations: number,
    logger?: AbstractLogger,
  ) => {
    if (this.instance) return;
    this.instance = new TxPotService(
      updateInterval,
      dataSource,
      txRequiredConfirmations,
      logger,
    );
  };

  /**
   * Returns the singleton instance.
   *
   * @returns TxPotService instance
   */
  static getInstance = (): TxPotService => {
    if (!this.instance) throw new Error('TxPotService instance is not initialized yet');
    return this.instance;
  };

  /**
   * Sets up tx-pot, registers the Ergo chain manager, and starts the update loop.
   *
   * @returns True when started
   */
  protected start = async (): Promise<boolean> => {
    TxPot.setup(this.dataSource, this.logger).registerChain(
      ERGO_CHAIN_NAME,
      new ErgoNetworkInterface(this.txRequiredConfirmations),
    );
    this.job();
    this.setStatus(ServiceStatus.running);
    return true;
  };

  /**
   * Stops the periodic tx-pot update loop.
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
   * Returns the global tx-pot instance.
   *
   * @returns TxPot instance
   */
  getTxPot = (): TxPot => {
    return TxPot.getInstance();
  };

  /**
   * Checks whether a work-item (identified by `extra`) is already enqueued in TxPot.
   *
   * @param txType - Cleanup tx type
   * @param extra - Unique work id (boxId)
   * @returns True when already enqueued
   */
  isEnqueued = async (txType: CleanupTxType, extra: string): Promise<boolean> => {
    const opts: TxOptions = {
      chain: ERGO_CHAIN_NAME,
      txType,
      extra,
    };
    const existing = await this.getTxPot().getTxsQuery([opts]);
    return existing.length > 0;
  };

  /**
   * Periodic job that runs `TxPot.update()`.
   */
  private job = async (): Promise<void> => {
    this.isJobRunning = true;
    await TxPot.getInstance().update();
    this.scheduledJob = setTimeout(this.job, this.updateInterval * 1000);
    this.isJobRunning = false;
    if (this.shouldStopJob) {
      this.shouldStopJob = false;
      this.continueStop();
    }
  };
}


