import { afterEach, describe, expect, it, vi } from 'vitest';
import { DataSource } from '@rosen-bridge/extended-typeorm';
import { Request } from '@ergo-raffle/box-lookup';

import { DBService } from '../src/services/dbService';
import { TxPotService } from '../src/services/txPotService';
import { BoxLookupService } from '../src/services/boxLookupService';
import { mockNodeUrl } from './testData';
import { resetServiceInstance } from './testUtils';
import {
  boxLookupCtorMock,
  boxLookupRegisterRequestMock,
  boxLookupServiceRequestsMock,
  boxLookupUnregisterRequestMock,
} from './mocked/BoxLookup.mock';

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
  resetServiceInstance(BoxLookupService);

  DBService.init(ds);
  await DBService.getInstance().startService();
  TxPotService.init(1, ds, 10);
  BoxLookupService.init(updateInterval, mockNodeUrl, undefined);
  return ds;
};

describe('startService', () => {
  /**
   * @target should schedule periodic box lookup request serving
   * @dependencies
   * - DBService started
   * - TxPotService initialized
   * @scenario
   * - Start box lookup service
   * - Advance timers
   * @expected
   * - serveRequests called at least once
   */
  it('should schedule periodic box lookup request serving', async () => {
    vi.useFakeTimers();
    // arrange
    const ds = await initServices(1);

    // act
    const ok = await BoxLookupService.getInstance().startService();
    vi.advanceTimersByTime(1100);
    await vi.runOnlyPendingTimersAsync();

    // assert
    expect(ok).toBe(true);
    expect(boxLookupServiceRequestsMock).toHaveBeenCalled();

    await ds.destroy();
    vi.useRealTimers();
  });
});

describe('stopService', () => {
  /**
   * @target should stop cleanly and cancel scheduled serving
   * @dependencies
   * - DBService started
   * - TxPotService initialized
   * @scenario
   * - Start box lookup service
   * - Stop it
   * @expected
   * - stopService resolves true
   * - serveRequests does not run again after stop
   */
  it('should stop cleanly and cancel scheduled serving', async () => {
    vi.useFakeTimers();
    const ds = await initServices(1);

    await BoxLookupService.getInstance().startService();
    await Promise.resolve();

    // act
    const ok = await BoxLookupService.getInstance().stopService();
    vi.advanceTimersByTime(2000);
    await vi.runOnlyPendingTimersAsync();

    // assert
    expect(ok).toBe(true);
    expect(boxLookupServiceRequestsMock).toHaveBeenCalledTimes(1);

    await ds.destroy();
    vi.useRealTimers();
  });
});

describe('addRequest', () => {
  /**
   * @target should delegate request registration to box-lookup
   * @dependencies
   * - BoxLookupService initialized
   * @scenario
   * - Add a request
   * @expected
   * - returns box-lookup request id
   */
  it('should delegate request registration to box-lookup', async () => {
    const ds = await initServices(10);

    const request: Request = {
      ergoTree: '0008',
      value: undefined,
      tokens: [],
      onSuffice: vi.fn().mockResolvedValue(undefined),
      getConfirmedBoxes: vi.fn().mockResolvedValue([]),
    };

    const id = BoxLookupService.getInstance().addRequest(request);
    expect(id).toBe(1);
    expect(boxLookupCtorMock).toHaveBeenCalledTimes(1);
    expect(boxLookupRegisterRequestMock).toHaveBeenCalledTimes(1);
    expect(boxLookupRegisterRequestMock).toHaveBeenCalledWith(request);

    await ds.destroy();
  });
});

describe('removeRequest', () => {
  /**
   * @target should delegate request unregistration to box-lookup
   * @dependencies
   * - BoxLookupService initialized
   * @scenario
   * - Remove a request
   * @expected
   * - unregisterRequest called with request id
   */
  it('should delegate request unregistration to box-lookup', async () => {
    const ds = await initServices(10);

    BoxLookupService.getInstance().removeRequest(7);
    expect(boxLookupCtorMock).toHaveBeenCalledTimes(1);
    expect(boxLookupUnregisterRequestMock).toHaveBeenCalledWith(7);

    await ds.destroy();
  });
});

afterEach(() => {
  vi.clearAllMocks();
  vi.useRealTimers();
});