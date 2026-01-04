import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { AbstractService, Dependency, ServiceStatus } from '@rosen-bridge/service-manager';
import { BoxLookup, Request } from '@ergo-raffle/box-lookup';

import { TxPotService } from './txPotService/txPotService';
import { deserializeTxForBoxLookup } from '../utils/boxLookupUtils';

export class BoxLookupService extends AbstractService {
  static name = 'BoxLookupService';
  name = BoxLookupService.name;
  private static instance?: BoxLookupService;

  protected dependencies: Dependency[] = [
    { serviceName: TxPotService.name, allowedStatuses: [ServiceStatus.running] },
  ];

  private boxLookup: BoxLookup;
  private readonly updateInterval: number;
  private isJobRunning = false;
  private scheduledJob?: NodeJS.Timeout;
  private shouldStopJob = false;
  private continueStop: () => void = () => undefined;
  private afterServeHandler: () => Promise<void> = async () => undefined;

  private constructor(updateInterval: number, nodeUrl: string, logger?: AbstractLogger) {
    super(logger);
    this.updateInterval = updateInterval;
    this.boxLookup = new BoxLookup(
      TxPotService.getInstance().getTxPot(),
      nodeUrl,
      deserializeTxForBoxLookup,
      this.logger,
      { after: () => this.afterServeHandler() },
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

  /**
   * Starts the periodic request serving loop.
   *
   * @returns True when started
   */
  protected start = async (): Promise<boolean> => {
    this.job();
    this.setStatus(ServiceStatus.running);
    return true;
  };

  /**
   * Stops the periodic request serving loop.
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
   * Registers a callback to run after each `BoxLookup.serveRequests()` run.
   */
  onAfterServe = (handler: () => Promise<void>): void => {
    this.afterServeHandler = handler;
  };

  /**
   * Periodic job that serves BoxLookup requests.
   */
  private job = async (): Promise<void> => {
    this.isJobRunning = true;
    try {
      await this.boxLookup.serveRequests();
    } catch (e) {
      this.logger.error(e instanceof Error ? e.message : String(e));
    }
    this.scheduledJob = setTimeout(this.job, this.updateInterval * 1000);
    this.isJobRunning = false;
    if (this.shouldStopJob) {
      this.shouldStopJob = false;
      this.continueStop();
    }
  };
}


