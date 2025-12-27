import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { AbstractService, Dependency, ServiceStatus } from '@rosen-bridge/service-manager';
import { DataSource } from '@rosen-bridge/extended-typeorm';

export class DBService extends AbstractService {
  static name = 'DBService';
  name = DBService.name;
  protected dependencies: Dependency[] = [];
  private static instance?: DBService;
  readonly dataSource: DataSource;

  private constructor(dataSource: DataSource, logger?: AbstractLogger) {
    super(logger);
    this.dataSource = dataSource;
  }

  /**
   * Initializes the singleton instance.
   *
   * @param dataSource - Configured TypeORM DataSource
   * @param logger - Optional logger
   */
  static init = (dataSource: DataSource, logger?: AbstractLogger) => {
    if (this.instance) return;
    this.instance = new DBService(dataSource, logger);
  };

  /**
   * Returns the singleton instance.
   *
   * @returns DBService instance
   */
  static getInstance = (): DBService => {
    if (!this.instance) throw new Error('DBService instance is not initialized yet');
    return this.instance;
  };

  /**
   * Initializes the DataSource and runs migrations.
   *
   * @returns True when started
   */
  protected start = async (): Promise<boolean> => {
    await this.dataSource.initialize();
    await this.dataSource.runMigrations();
    this.setStatus(ServiceStatus.running);
    return true;
  };

  /**
   * Stops the service (DataSource lifecycle is managed externally for now).
   *
   * @returns True when stopped
   */
  protected stop = async (): Promise<boolean> => {
    this.setStatus(ServiceStatus.dormant);
    return true;
  };
}


