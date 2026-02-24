import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { Dependency, PeriodicTaskService, ServiceStatus } from '@rosen-bridge/service-manager';
import { BoxLookup, Request } from '@ergo-raffle/box-lookup';

import { TxPotService } from './txPotService/txPotService';
import { deserializeTxForBoxLookup } from '../utils/boxLookupUtils';

export class BoxLookupService extends PeriodicTaskService {
  static name = 'BoxLookupService';
  protected name = BoxLookupService.name;
  taskName = 'BoxLookupServiceTask';
  private static instance?: BoxLookupService;

  protected dependencies: Dependency[] = [
    { serviceName: TxPotService.name, allowedStatuses: [ServiceStatus.running] },
  ];

  private boxLookup: BoxLookup;
  private readonly updateInterval: number;

  private constructor(updateInterval: number, nodeUrl: string, logger?: AbstractLogger) {
    super(logger);
    this.updateInterval = updateInterval;
    this.boxLookup = new BoxLookup(
      TxPotService.getInstance().getTxPot(),
      nodeUrl,
      deserializeTxForBoxLookup,
      this.logger
    );
  }

  /**
   * Initializes the singleton instance.
   *
   * @param updateInterval - Interval in seconds
   * @param nodeUrl - Ergo node URL
   * @param logger - Optional logger
   */
  static init = (updateInterval: number, nodeUrl: string, logger?: AbstractLogger) => {
    if (this.instance) return;
    this.instance = new BoxLookupService(updateInterval, nodeUrl, logger);
  };

  /**
   * Returns the singleton instance.
   *
   * @returns BoxLookupService instance
   */
  static getInstance = (): BoxLookupService => {
    if (!this.instance) throw new Error('BoxLookupService instance is not initialized yet');
    return this.instance;
  };

  protected preStart = async (): Promise<void> => {
    return;
  };

  protected postStop = async (): Promise<void> => {
    return;
  };

  protected getTasks = () => {
    return [
      {
        fn: this.serveRequests,
        interval: this.updateInterval * 1000,
      },
    ];
  };

  /**
   * Registers a request with the underlying BoxLookup instance.
   *
   * @param request - Request definition
   * @returns Request id
   */
  addRequest = (request: Request): number => {
    return this.boxLookup.registerRequest(request);
  };

  /**
   * Unregisters a request from the underlying BoxLookup instance.
   *
   * @param requestId - Previously returned request id
   */
  removeRequest = (requestId?: number) => {
    if (requestId === undefined) return;
    this.boxLookup.unregisterRequest(requestId);
  };

  /**
   * Serves requests from the underlying BoxLookup instance.
   */
  private serveRequests = async (): Promise<void> => {
    try {
      await this.boxLookup.serveRequests();
    } catch (e) {
      this.logger.error(e instanceof Error ? e.message : String(e));
    }
  };
}


