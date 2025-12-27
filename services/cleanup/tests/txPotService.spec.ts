import { afterEach, describe, expect, it, vi } from 'vitest';
import { DataSource } from '@rosen-bridge/extended-typeorm';

import { TxPotService } from '../src/services/txPotService';
import { DBService } from '../src/services/dbService';
import { resetServiceInstance } from './testUtils';
import {
  txPotGetInstanceMock,
  txPotRegisterChainMock,
  txPotSetupMock,
  txPotUpdateMock,
} from './mocked/TxPot.mock';

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

  DBService.init(ds);
  await DBService.getInstance().startService();
  TxPotService.init(updateInterval, ds, 10);
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

    // assert
    expect(ok).toBe(true);
    expect(txPotSetupMock).toHaveBeenCalledTimes(1);
    expect(txPotRegisterChainMock).toHaveBeenCalledTimes(1);
    expect(txPotGetInstanceMock).toHaveBeenCalled();
    expect(txPotUpdateMock).toHaveBeenCalled();

    await ds.destroy();
  });
});

describe('stopService', () => {
  /**
   * @target should stop cleanly and cancel scheduled updates
   * @dependencies
   * - DBService started
   * @scenario
   * - Start TxPotService
   * - Stop it
   * @expected
   * - stopService resolves true
   * - update does not run again after stop
   */
  it('should stop cleanly and cancel scheduled updates', async () => {
    vi.useFakeTimers();
    const ds = await initServices(1);

    await TxPotService.getInstance().startService();

    // act
    const ok = await TxPotService.getInstance().stopService();
    vi.advanceTimersByTime(2000);
    await vi.runOnlyPendingTimersAsync();

    // assert
    expect(ok).toBe(true);
    expect(txPotUpdateMock).toHaveBeenCalledTimes(1);
    await ds.destroy();
  });
});

afterEach(() => {
  vi.clearAllMocks();
  vi.useRealTimers();
});