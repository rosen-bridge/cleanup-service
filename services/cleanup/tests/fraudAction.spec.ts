import { beforeEach, describe, expect, it, vi } from 'vitest';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { OutputBox } from '@ergo-raffle/box-lookup';

import '../src/bootstrap';
import './mocked/fraudAction.mock';
import './mocked/cleanupUtils.mock';
import {
  workflowContracts,
  workflowTriggerEventOutputBox2,
  workflowCleanerBoxJson,
  workflowFeeBoxesJson,
  signedFraudTxJson,
} from './testData';

import { FraudAction } from '../src/services/cleanupService/fraudAction';
import { CleanupTxType, RosenContracts } from '../src/types';
import { BoxLookupService } from '../src/services/boxLookupService';
import { ScannerService } from '../src/services/scannerService';
import { getLogger } from '../src/configs/loggerConfig';

describe('fraudAction', () => {
  const contracts = workflowContracts as RosenContracts;
  const cleanupAddress = workflowCleanerBoxJson.address;
  const logger = getLogger(import.meta.url);

  let addRequestMock: ReturnType<typeof vi.fn>;
  let removeRequestMock: ReturnType<typeof vi.fn>;
  let getCurrentHeightMock: ReturnType<typeof vi.fn>;
  let getUnspentTriggerBoxesMock: ReturnType<typeof vi.fn>;
  let getTriggerWidsByTxIdMock: ReturnType<typeof vi.fn>;
  let getUnspentBoxesByAddressMock: ReturnType<typeof vi.fn>;
  let signAndEnqueueTxMock: ReturnType<typeof vi.fn>;

  const makeMockTx = (outputs: OutputBox[]): ergoLib.Transaction => {
    const tx = Object.create(
      ergoLib.Transaction.prototype,
    ) as ergoLib.Transaction & { __outputs: OutputBox[] };
    tx.__outputs = outputs;
    return tx;
  };

  beforeEach(() => {
    addRequestMock = vi.fn(() => 11);
    removeRequestMock = vi.fn();
    const boxLookupMock = Object.create(
      BoxLookupService.prototype,
    ) as BoxLookupService;
    boxLookupMock.addRequest = addRequestMock;
    boxLookupMock.removeRequest = removeRequestMock;
    vi.spyOn(BoxLookupService, 'getInstance').mockReturnValue(boxLookupMock);

    getCurrentHeightMock = vi.fn().mockResolvedValue(2000000);
    getUnspentTriggerBoxesMock = vi.fn().mockResolvedValue([]);
    getTriggerWidsByTxIdMock = vi.fn().mockResolvedValue([]);
    getUnspentBoxesByAddressMock = vi.fn().mockResolvedValue([]);
    const scannerMock = Object.create(
      ScannerService.prototype,
    ) as ScannerService;
    scannerMock.getCurrentHeight = getCurrentHeightMock;
    scannerMock.getUnspentTriggerBoxes = getUnspentTriggerBoxesMock;
    scannerMock.getTriggerWidsByTxId = getTriggerWidsByTxIdMock;
    scannerMock.getUnspentBoxesByAddress = getUnspentBoxesByAddressMock;
    vi.spyOn(ScannerService, 'getInstance').mockReturnValue(scannerMock);

    signAndEnqueueTxMock = vi
      .fn()
      .mockResolvedValue(makeMockTx(signedFraudTxJson.outputs as OutputBox[]));

    vi.clearAllMocks();
  });

  const invokeOnSuffice = async (
    action: FraudAction,
    boxes: OutputBox[],
    unspentBoxes: OutputBox[],
  ): Promise<void> => {
    action.register();
    const request = addRequestMock.mock.calls.at(-1)?.[0];
    await request.onSuffice(boxes, unspentBoxes, 1);
  };

  /**
   * @target should register and unregister trigger request
   * @dependencies
   * - BoxLookupService mocked
   * @scenario
   * - Register action request
   * - Unregister action request
   * @expected
   * - addRequest called once
   * - removeRequest called with registered id
   */
  it('should register and unregister trigger request', () => {
    const action = new FraudAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    action.register();
    action.unregister();

    expect(addRequestMock).toHaveBeenCalledTimes(1);
    expect(removeRequestMock).toHaveBeenCalledWith(11);
  });

  /**
   * @target should filter confirmed trigger boxes to expired ones
   * @dependencies
   * - ScannerService mocked
   * @scenario
   * - Register trigger request
   * - Call request.getConfirmedBoxes
   * @expected
   * - only expired trigger boxes are returned
   */
  it('should filter confirmed trigger boxes to expired ones', async () => {
    const nonExpired = {
      ...workflowTriggerEventOutputBox2,
    } as OutputBox;
    nonExpired.creationHeight = 2000001;
    const expired = {
      ...workflowTriggerEventOutputBox2,
      transactionId: `${workflowTriggerEventOutputBox2.transactionId}-2`,
    } as OutputBox;
    expired.creationHeight = 10;
    getUnspentTriggerBoxesMock.mockResolvedValue([nonExpired, expired]);

    const action = new FraudAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    action.register();

    const request = addRequestMock.mock.calls[0]?.[0];
    const boxes = await request.getConfirmedBoxes();
    expect(boxes.map((b: OutputBox) => b.creationHeight)).toEqual([10]);
  });

  /**
   * @target should skip when trigger is not expired
   * @dependencies
   * - ScannerService mocked
   * @scenario
   * - Call onTriggerEventSuffice with non-expired trigger
   * @expected
   * - signAndEnqueueTx is not called
   */
  it('should skip when trigger is not expired', async () => {
    getCurrentHeightMock.mockResolvedValue(1695386);

    const action = new FraudAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    const cleanupUnspent = [
      workflowCleanerBoxJson as OutputBox,
      ...(workflowFeeBoxesJson as OutputBox[]),
    ];
    const trigger = workflowTriggerEventOutputBox2 as OutputBox;
    await invokeOnSuffice(action, [trigger], cleanupUnspent);

    expect(signAndEnqueueTxMock).not.toHaveBeenCalled();
  });

  /**
   * @target should use unspent boxes first and enqueue fraud tx
   * @dependencies
   * - ScannerService mocked
   * @scenario
   * - Provide cleanup inputs in unspent boxes
   * - Call onTriggerEventSuffice with valid trigger
   * @expected
   * - scanner fallback is not called
   * - signAndEnqueueTx is called once
   */
  it('should use unspent boxes first and enqueue fraud tx', async () => {
    const trigger = workflowTriggerEventOutputBox2 as OutputBox;
    const commitmentCount = 2;
    getTriggerWidsByTxIdMock.mockResolvedValue(
      Array.from({ length: commitmentCount }, (_, i) => `wid-${i}`),
    );
    const cleanupUnspent = [
      workflowCleanerBoxJson as OutputBox,
      ...(workflowFeeBoxesJson as OutputBox[]),
    ];

    const action = new FraudAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    await invokeOnSuffice(action, [trigger], cleanupUnspent);

    expect(getUnspentBoxesByAddressMock).not.toHaveBeenCalled();
    expect(signAndEnqueueTxMock).toHaveBeenCalledTimes(1);
    expect(signAndEnqueueTxMock).toHaveBeenCalledWith(
      CleanupTxType.fraud,
      expect.anything(),
      expect.any(Array),
      2000000,
    );
  });

  /**
   * @target should fallback to scanner when cleanup inputs are missing in unspent
   * @dependencies
   * - ScannerService mocked
   * @scenario
   * - Call onTriggerEventSuffice without cleanup inputs in unspent
   * @expected
   * - scanner fallback is called
   * - signAndEnqueueTx is called once
   */
  it('should fallback to scanner when cleanup inputs are missing in unspent', async () => {
    const trigger = workflowTriggerEventOutputBox2 as OutputBox;
    const commitmentCount = 2;
    getTriggerWidsByTxIdMock.mockResolvedValue(
      Array.from({ length: commitmentCount }, (_, i) => `wid-${i}`),
    );
    getUnspentBoxesByAddressMock.mockResolvedValue([
      workflowCleanerBoxJson as OutputBox,
      ...(workflowFeeBoxesJson as OutputBox[]),
    ]);

    const action = new FraudAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    await invokeOnSuffice(action, [trigger], []);

    expect(getUnspentBoxesByAddressMock).toHaveBeenCalledWith(cleanupAddress);
    expect(signAndEnqueueTxMock).toHaveBeenCalledTimes(1);
  });

  /**
   * @target should create multiple fraud transactions for multiple trigger boxes
   * @dependencies
   * - ScannerService mocked
   * @scenario
   * - Call onTriggerEventSuffice with two trigger boxes
   * @expected
   * - signAndEnqueueTx is called twice
   */
  it('should create multiple fraud transactions for multiple trigger boxes', async () => {
    const trigger = workflowTriggerEventOutputBox2 as OutputBox;
    const commitmentCount = 2;
    getTriggerWidsByTxIdMock.mockResolvedValue(
      Array.from({ length: commitmentCount }, (_, i) => `wid-${i}`),
    );
    const cleanupUnspent = [
      workflowCleanerBoxJson as OutputBox,
      ...(workflowFeeBoxesJson as OutputBox[]),
    ];

    const action = new FraudAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    await invokeOnSuffice(action, [trigger, trigger], cleanupUnspent);

    expect(signAndEnqueueTxMock).toHaveBeenCalledTimes(2);
  });
});
