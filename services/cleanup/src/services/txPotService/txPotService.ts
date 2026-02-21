import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import {
  Dependency,
  PeriodicTaskService,
  ServiceStatus,
} from '@rosen-bridge/service-manager';
import { TxPot } from '@rosen-bridge/tx-pot';
import { DataSource } from '@rosen-bridge/extended-typeorm';
import { DBService } from '../dbService';
import { ERGO_CHAIN_NAME } from '../../configs/constants';
import { ErgoNetworkInterface } from './ergoNetworkInterface';

export class TxPotService extends PeriodicTaskService {
  static name = 'TxPotService';
  protected name = TxPotService.name;
  taskName = 'TxPotServiceTask';
  private static instance?: TxPotService;

  protected dependencies: Dependency[] = [
    { serviceName: DBService.name, allowedStatuses: [ServiceStatus.running] },
  ];

  private ergoNetworkInterface?: ErgoNetworkInterface;

  private constructor(
    private updateInterval: number,
    private dataSource: DataSource,
    private txRequiredConfirmations: number,
    logger?: AbstractLogger,
  ) {
    super(logger);
    TxPot.setup(this.dataSource, this.logger);
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
    if (!this.instance)
      throw new Error('TxPotService instance is not initialized yet');
    return this.instance;
  };

  /**
   * Sets up tx-pot and registers the Ergo chain manager.
   */
  protected preStart = async (): Promise<void> => {
    this.ergoNetworkInterface = new ErgoNetworkInterface(
      this.txRequiredConfirmations,
      this.logger,
    );
    TxPot.getInstance().registerChain(
      ERGO_CHAIN_NAME,
      this.ergoNetworkInterface,
    );
  };

  protected postStop = async (): Promise<void> => {
    this.ergoNetworkInterface = undefined;
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
   * Returns the Ergo network interface.
   *
   * @returns Ergo network interface
   */
  getErgoNetworkInterface = (): ErgoNetworkInterface => {
    if (!this.ergoNetworkInterface) {
      throw new Error('TxPotService not started yet');
    }
    return this.ergoNetworkInterface;
  };

  protected getTasks = () => {
    return [
      {
        fn: this.updateTxPot,
        interval: this.updateInterval * 1000,
      },
    ];
  };

  private updateTxPot = async (): Promise<void> => {
    await TxPot.getInstance().update();
  };
}
