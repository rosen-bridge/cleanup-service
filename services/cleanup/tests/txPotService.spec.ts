import { DataSource } from 'typeorm';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { ServiceManager } from '@rosen-bridge/service-manager';

import {
  txPotGetInstanceMock,
  txPotRegisterChainMock,
  txPotSetupMock,
  txPotUpdateMock,
} from './mocked/txPot.mock';

import { DBService } from '../src/services/dbService';
import { TxPotService } from '../src/services/txPotService/txPotService';
import { resetServiceInstance } from './testUtils';

const createMemoryDataSource = () =>
  new DataSource({
    type: 'sqlite',
    database: ':memory:',
    entities: [],
    migrations: [],
    synchronize: false,
    logging: false,
  });

const initServices = async (updateInterval: number) => {
  const ds = createMemoryDataSource();
  resetServiceInstance(DBService);
  resetServiceInstance(TxPotService);

  const serviceManager = ServiceManager.setup();
  DBService.init(ds);
  serviceManager.register(DBService.getInstance());
  await DBService.getInstance().startService();
  TxPotService.init(updateInterval, ds, 10);
  serviceManager.register(TxPotService.getInstance());
  return ds;
};

describe('startService', () => {
  /**
   * @target should register ergo chain and schedule periodic updates
   * @dependencies
   * - DBService started
   * @scenario
   * - Initialize TxPotService
   * - Start service
   * - Advance timers
   * @expected
   * - TxPot.setup called once
   * - TxPot.getInstance().update called at least once
   */
  it('should register ergo chain and schedule periodic updates', async () => {
    vi.useFakeTimers();
    // arrange
    const ds = await initServices(1);

    // act
    const ok = await TxPotService.getInstance().startService();
    vi.advanceTimersByTime(1100);
    await vi.runOnlyPendingTimersAsync();

    expect(ok).toBe(true);
    expect(txPotSetupMock).toHaveBeenCalledTimes(1);
    expect(txPotRegisterChainMock).toHaveBeenCalledTimes(1);
    expect(txPotGetInstanceMock).toHaveBeenCalled();
    expect(txPotUpdateMock).toHaveBeenCalled();

    await ds.destroy();
  });
});

afterEach(() => {
  vi.clearAllMocks();
  vi.useRealTimers();
});
