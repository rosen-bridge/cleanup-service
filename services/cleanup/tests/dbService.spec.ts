import { DataSource } from '@rosen-bridge/extended-typeorm';
import { describe, expect, it } from 'vitest';

import '../src/bootstrap';
import entities from '../src/db/entities';
import migrations from '../src/db/migrations';
import { DBService } from '../src/services/dbService';
import { resetServiceInstance } from './testUtils';

describe('startService', () => {
  /**
   * @target should initialize and migrate datasource
   * @dependencies
   * - sqlite3
   * @scenario
   * - Create an in-memory datasource
   * - Initialize DBService with it
   * - Start service and verify datasource is initialized
   * @expected
   * - dataSource.isInitialized becomes true
   */
  it('should initialize and migrate datasource', async () => {
    // arrange
    const ds = new DataSource({
      type: 'sqlite',
      database: ':memory:',
      entities: [],
      migrations: [],
      synchronize: false,
      logging: false,
    });
    resetServiceInstance(DBService);
    DBService.init(ds);

    // act
    const ok = await DBService.getInstance().startService();

    // assert
    expect(ok).toBe(true);
    expect(ds.isInitialized).toBe(true);
    await ds.destroy();
  });

  /**
   * @target should initialize and run all sqlite migrations
   * @dependencies
   * - sqlite3
   * @scenario
   * - Create an in-memory datasource with cleanup-service entities+migrations
   * - Start DBService
   * @expected
   * - startService succeeds and datasource is initialized
   */
  it('should initialize and run all sqlite migrations', async () => {
    // arrange
    const ds = new DataSource({
      type: 'sqlite',
      database: ':memory:',
      entities,
      migrations: migrations.sqlite,
      synchronize: false,
      logging: false,
    });
    resetServiceInstance(DBService);
    DBService.init(ds);

    // act
    const ok = await DBService.getInstance().startService();

    // assert
    expect(ok).toBe(true);
    expect(ds.isInitialized).toBe(true);
    await ds.destroy();
  });
});
