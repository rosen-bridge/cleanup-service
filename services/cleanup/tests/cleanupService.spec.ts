import { BoxEntity } from '@rosen-bridge/address-extractor';
import { FraudEntity } from '@rosen-bridge/fraud-extractor';
import { TransactionEntity } from '@rosen-bridge/tx-pot';
import {
  CollateralEntity,
  CommitmentEntity,
  EventTriggerEntity,
} from '@rosen-bridge/watcher-data-extractor';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { DataSource } from 'typeorm';
import { afterEach, describe, expect, it, vi } from 'vitest';

import {
  boxLookupRegisterRequestMock,
  boxLookupUnregisterRequestMock,
} from './mocked/boxLookup.mock';
import './mocked/ergoNodeNetwork.mock';

import '../src/bootstrap';
import { BoxLookupService } from '../src/services/boxLookupService';
import { CleanupService } from '../src/services/cleanupService';
import { DBService } from '../src/services/dbService';
import { ScannerService } from '../src/services/scannerService';
import { TxPotService } from '../src/services/txPotService/txPotService';
import { mockExplorerUrl, mockNodeUrl, workflowContracts } from './testData';
import { resetServiceInstance } from './testUtils';

const testEntities = [
  TransactionEntity,
  BoxEntity,
  EventTriggerEntity,
  FraudEntity,
  CollateralEntity,
  CommitmentEntity,
];

describe('cleanupService', () => {
  afterEach(() => {
    vi.clearAllMocks();
  });

  /**
   * @target should register fraud and slash box-lookup requests on start
   * @dependencies
   * - DBService started
   * - ScannerService started
   * - TxPotService started
   * - BoxLookupService started
   * @scenario
   * - Start CleanupService
   * @expected
   * - BoxLookup.registerRequest called twice
   */
  it('should register fraud and slash box-lookup requests on start', async () => {
    vi.useFakeTimers();

    const ds = new DataSource({
      type: 'sqlite',
      database: ':memory:',
      entities: testEntities,
      migrations: [],
      synchronize: true,
      logging: false,
    });

    resetServiceInstance(DBService);
    resetServiceInstance(ScannerService);
    resetServiceInstance(TxPotService);
    resetServiceInstance(BoxLookupService);
    resetServiceInstance(CleanupService);
    boxLookupRegisterRequestMock.mockClear();

    DBService.init(ds);
    await DBService.getInstance().startService();
    ScannerService.init(
      1,
      mockNodeUrl,
      mockExplorerUrl,
      0,
      ergoLib.NetworkPrefix.Mainnet,
      workflowContracts,
      [],
      undefined,
    );
    await ScannerService.getInstance().startService();
    TxPotService.init(1, ds, 10);
    await TxPotService.getInstance().startService();
    BoxLookupService.init(1, mockNodeUrl, undefined);
    await BoxLookupService.getInstance().startService();

    CleanupService.init();
    const ok = await CleanupService.getInstance().startService();

    expect(ok).toBe(true);
    expect(boxLookupRegisterRequestMock).toHaveBeenCalledTimes(2);

    await ds.destroy();
    vi.useRealTimers();
  });

  /**
   * @target should unregister fraud and slash box-lookup requests on stop
   * @dependencies
   * - CleanupService started
   * @scenario
   * - Start CleanupService
   * - Stop CleanupService
   * @expected
   * - BoxLookup.unregisterRequest called twice
   */
  it('should unregister fraud and slash box-lookup requests on stop', async () => {
    const ds = new DataSource({
      type: 'sqlite',
      database: ':memory:',
      entities: testEntities,
      migrations: [],
      synchronize: true,
      logging: false,
    });

    resetServiceInstance(DBService);
    resetServiceInstance(ScannerService);
    resetServiceInstance(TxPotService);
    resetServiceInstance(BoxLookupService);
    resetServiceInstance(CleanupService);
    boxLookupUnregisterRequestMock.mockClear();

    DBService.init(ds);
    await DBService.getInstance().startService();
    ScannerService.init(
      1,
      mockNodeUrl,
      mockExplorerUrl,
      0,
      ergoLib.NetworkPrefix.Mainnet,
      workflowContracts,
      [],
      undefined,
    );
    await ScannerService.getInstance().startService();
    TxPotService.init(1, ds, 10);
    await TxPotService.getInstance().startService();
    BoxLookupService.init(1, mockNodeUrl, undefined);
    await BoxLookupService.getInstance().startService();

    CleanupService.init();
    await CleanupService.getInstance().startService();
    await CleanupService.getInstance().stopService();

    expect(boxLookupUnregisterRequestMock).toHaveBeenCalledTimes(2);

    await ds.destroy();
  });
});
