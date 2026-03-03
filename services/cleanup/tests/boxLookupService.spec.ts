import '../src/bootstrap';

import { Request } from '@ergo-raffle/box-lookup';
import { afterEach, describe, expect, it, vi } from 'vitest';

import {
  boxLookupCtorMock,
  boxLookupRegisterRequestMock,
  boxLookupUnregisterRequestMock,
} from './mocked/boxLookup.mock';
import './mocked/ergoNodeNetwork.mock';

import { BoxLookupService } from '../src/services/boxLookupService';
import { DBService } from '../src/services/dbService';
import { TxPotService } from '../src/services/txPotService/txPotService';
import { mockNodeUrl } from './testData';
import { createMemoryDataSource, resetServiceInstance } from './testUtils';

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
