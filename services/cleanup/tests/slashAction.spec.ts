import { OutputBox } from '@ergo-raffle/box-lookup';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { beforeEach, describe, expect, it, vi } from 'vitest';

import './mocked/cleanupUtils.mock';
import './mocked/slashAction.mock';

import '../src/bootstrap';
import { getLogger } from '../src/config/loggerConfig';
import { BoxLookupService } from '../src/services/boxLookupService';
import { SlashAction } from '../src/services/cleanupService/slashAction';
import { ScannerService } from '../src/services/scannerService';
import { CleanupTxType, RosenContracts } from '../src/types';
import {
  workflowContracts,
  workflowFraudOutputBox,
  workflowCleanerBoxJson,
  workflowFeeBoxesJson,
  workflowRepoOutputBox,
  workflowCollateralOutputBox,
  signedSlashTxJson,
} from './testData';

describe('slashAction', () => {
  const contracts = workflowContracts as RosenContracts;
  const cleanupAddress = workflowCleanerBoxJson.address;
  const logger = getLogger(import.meta.url);

  let addRequestMock: ReturnType<typeof vi.fn>;
  let removeRequestMock: ReturnType<typeof vi.fn>;
  let getCurrentHeightMock: ReturnType<typeof vi.fn>;
  let getUnspentFraudBoxesMock: ReturnType<typeof vi.fn>;
  let getUnspentBoxesByAddressMock: ReturnType<typeof vi.fn>;
  let getUnspentCollateralBoxesMock: ReturnType<typeof vi.fn>;
  let signAndEnqueueTxMock: ReturnType<typeof vi.fn>;

  const makeMockTx = (outputs: OutputBox[]): ergoLib.Transaction => {
    const tx = Object.create(
      ergoLib.Transaction.prototype,
    ) as ergoLib.Transaction & { __outputs: OutputBox[] };
    tx.__outputs = outputs;
    return tx;
  };

  const invokeOnSuffice = async (
    action: SlashAction,
    boxes: OutputBox[],
    unspentBoxes: OutputBox[],
  ): Promise<void> => {
    action.register();
    const request = addRequestMock.mock.calls.at(-1)?.[0];
    await request.onSuffice(boxes, unspentBoxes, 1);
  };

  beforeEach(() => {
    addRequestMock = vi.fn(() => 21);
    removeRequestMock = vi.fn();
    const boxLookupMock = Object.create(
      BoxLookupService.prototype,
    ) as BoxLookupService;
    boxLookupMock.addRequest = addRequestMock;
    boxLookupMock.removeRequest = removeRequestMock;
    vi.spyOn(BoxLookupService, 'getInstance').mockReturnValue(boxLookupMock);

    getCurrentHeightMock = vi.fn().mockResolvedValue(2000000);
    getUnspentFraudBoxesMock = vi.fn().mockResolvedValue([]);
    getUnspentBoxesByAddressMock = vi.fn().mockResolvedValue([]);
    getUnspentCollateralBoxesMock = vi.fn().mockResolvedValue([]);
    const scannerMock = Object.create(
      ScannerService.prototype,
    ) as ScannerService;
    scannerMock.getCurrentHeight = getCurrentHeightMock;
    scannerMock.getUnspentFraudBoxes = getUnspentFraudBoxesMock;
    scannerMock.getUnspentBoxesByAddress = getUnspentBoxesByAddressMock;
    scannerMock.getUnspentCollateralBoxes = getUnspentCollateralBoxesMock;
    vi.spyOn(ScannerService, 'getInstance').mockReturnValue(scannerMock);

    signAndEnqueueTxMock = vi
      .fn()
      .mockResolvedValue(makeMockTx(signedSlashTxJson.outputs as OutputBox[]));

    vi.clearAllMocks();
  });

  /**
   * @target should register and unregister fraud request
   * @dependencies
   * - BoxLookupService mocked
   * @scenario
   * - Register action request
   * - Unregister action request
   * @expected
   * - addRequest called once
   * - removeRequest called with registered id
   */
  it('should register and unregister fraud request', () => {
    const action = new SlashAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    action.register();
    action.unregister();

    expect(addRequestMock).toHaveBeenCalledTimes(1);
    expect(removeRequestMock).toHaveBeenCalledWith(21);
  });

  /**
   * @target should use unspent boxes first and enqueue slash tx
   * @dependencies
   * - ScannerService mocked
   * @scenario
   * - Provide cleanup, repo, and collateral inputs in unspent boxes
   * - Call onFraudBoxSuffice with valid fraud box
   * @expected
   * - scanner fallbacks are not called
   * - signAndEnqueueTx is called once
   */
  it('should use unspent boxes first and enqueue slash tx', async () => {
    const fraud = workflowFraudOutputBox as OutputBox;
    const unspentBoxes = [
      workflowCleanerBoxJson as OutputBox,
      ...(workflowFeeBoxesJson as OutputBox[]),
      workflowRepoOutputBox as OutputBox,
      workflowCollateralOutputBox as OutputBox,
    ];

    const action = new SlashAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    await invokeOnSuffice(action, [fraud], unspentBoxes);

    expect(getUnspentBoxesByAddressMock).not.toHaveBeenCalled();
    expect(getUnspentCollateralBoxesMock).not.toHaveBeenCalled();
    expect(signAndEnqueueTxMock).toHaveBeenCalledTimes(1);
    expect(signAndEnqueueTxMock).toHaveBeenCalledWith(
      CleanupTxType.slash,
      expect.anything(),
      expect.any(Array),
      2000000,
    );
  });

  /**
   * @target should fallback to scanner when shared inputs are missing in unspent
   * @dependencies
   * - ScannerService mocked
   * @scenario
   * - Call onFraudBoxSuffice without cleanup, repo, and collateral in unspent
   * @expected
   * - scanner fallbacks are called
   * - signAndEnqueueTx is called once
   */
  it('should fallback to scanner when shared inputs are missing in unspent', async () => {
    const fraud = workflowFraudOutputBox as OutputBox;
    getUnspentBoxesByAddressMock.mockImplementation(async (address: string) => {
      if (address === cleanupAddress) {
        return [
          workflowCleanerBoxJson as OutputBox,
          ...(workflowFeeBoxesJson as OutputBox[]),
        ];
      }
      if (address === contracts.addresses.RWTRepo) {
        return [workflowRepoOutputBox as OutputBox];
      }
      return [];
    });
    getUnspentCollateralBoxesMock.mockResolvedValue([
      workflowCollateralOutputBox as OutputBox,
    ]);

    const action = new SlashAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    await invokeOnSuffice(action, [fraud], []);

    expect(getUnspentBoxesByAddressMock).toHaveBeenCalledWith(cleanupAddress);
    expect(getUnspentBoxesByAddressMock).toHaveBeenCalledWith(
      contracts.addresses.RWTRepo,
    );
    expect(getUnspentCollateralBoxesMock).toHaveBeenCalledTimes(1);
    expect(signAndEnqueueTxMock).toHaveBeenCalledTimes(1);
  });

  /**
   * @target should skip slash tx when collateral is missing
   * @dependencies
   * - ScannerService mocked
   * @scenario
   * - Provide cleanup and repo in unspent boxes
   * - Call onFraudBoxSuffice with no collateral box
   * @expected
   * - signAndEnqueueTx is not called
   */
  it('should skip slash tx when collateral is missing', async () => {
    const fraud = workflowFraudOutputBox as OutputBox;
    const unspentBoxes = [
      workflowCleanerBoxJson as OutputBox,
      ...(workflowFeeBoxesJson as OutputBox[]),
      workflowRepoOutputBox as OutputBox,
    ];

    const action = new SlashAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    await invokeOnSuffice(action, [fraud], unspentBoxes);

    expect(signAndEnqueueTxMock).not.toHaveBeenCalled();
  });

  /**
   * @target should skip slash tx when cleanup inputs are missing
   * @dependencies
   * - ScannerService mocked
   * @scenario
   * - Provide repo and collateral in unspent boxes
   * - Call onFraudBoxSuffice without cleanup inputs
   * @expected
   * - cleanup fallback is checked
   * - signAndEnqueueTx is not called
   */
  it('should skip slash tx when cleanup inputs are missing', async () => {
    const fraud = workflowFraudOutputBox as OutputBox;
    const unspentBoxes = [
      workflowRepoOutputBox as OutputBox,
      workflowCollateralOutputBox as OutputBox,
    ];

    const action = new SlashAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    await invokeOnSuffice(action, [fraud], unspentBoxes);

    expect(getUnspentBoxesByAddressMock).toHaveBeenCalledWith(cleanupAddress);
    expect(signAndEnqueueTxMock).not.toHaveBeenCalled();
  });

  /**
   * @target should skip slash tx when repo is missing
   * @dependencies
   * - ScannerService mocked
   * @scenario
   * - Provide cleanup and collateral in unspent boxes
   * - Call onFraudBoxSuffice without repo inputs
   * @expected
   * - repo fallback is checked
   * - signAndEnqueueTx is not called
   */
  it('should skip slash tx when repo is missing', async () => {
    const fraud = workflowFraudOutputBox as OutputBox;
    const unspentBoxes = [
      workflowCleanerBoxJson as OutputBox,
      ...(workflowFeeBoxesJson as OutputBox[]),
      workflowCollateralOutputBox as OutputBox,
    ];

    const action = new SlashAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    await invokeOnSuffice(action, [fraud], unspentBoxes);

    expect(getUnspentBoxesByAddressMock).toHaveBeenCalledWith(
      contracts.addresses.RWTRepo,
    );
    expect(signAndEnqueueTxMock).not.toHaveBeenCalled();
  });

  /**
   * @target should handle collateral per wid when fraud boxes have different wids
   * @dependencies
   * - ScannerService mocked
   * @scenario
   * - Provide two fraud boxes with different wids
   * - Provide matching collateral box for each wid in unspent boxes
   * @expected
   * - signAndEnqueueTx is called twice
   */
  it('should handle collateral per wid when fraud boxes have different wids', async () => {
    const fraudA = workflowFraudOutputBox as OutputBox;
    const fraudB = {
      ...workflowFraudOutputBox,
      transactionId: `${workflowFraudOutputBox.transactionId}-2`,
      additionalRegisters: {
        ...workflowFraudOutputBox.additionalRegisters,
        R4: 'wid-2',
      },
    } as OutputBox;
    const collateralA = workflowCollateralOutputBox as OutputBox;
    const collateralB = {
      ...workflowCollateralOutputBox,
      transactionId: `${workflowCollateralOutputBox.transactionId}-2`,
      additionalRegisters: {
        ...workflowCollateralOutputBox.additionalRegisters,
        R4: 'wid-2',
      },
    } as OutputBox;
    const unspentBoxes = [
      workflowCleanerBoxJson as OutputBox,
      ...(workflowFeeBoxesJson as OutputBox[]),
      workflowRepoOutputBox as OutputBox,
      collateralA,
      collateralB,
    ];

    const action = new SlashAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    await invokeOnSuffice(action, [fraudA, fraudB], unspentBoxes);

    expect(signAndEnqueueTxMock).toHaveBeenCalledTimes(2);
  });

  /**
   * @target should create multiple slash transactions for multiple fraud boxes
   * @dependencies
   * - ScannerService mocked
   * @scenario
   * - Call onFraudBoxSuffice with two fraud boxes
   * @expected
   * - signAndEnqueueTx is called twice
   */
  it('should create multiple slash transactions for multiple fraud boxes', async () => {
    const fraud = workflowFraudOutputBox as OutputBox;
    const unspentBoxes = [
      workflowCleanerBoxJson as OutputBox,
      ...(workflowFeeBoxesJson as OutputBox[]),
      workflowRepoOutputBox as OutputBox,
      workflowCollateralOutputBox as OutputBox,
    ];

    const action = new SlashAction(
      logger,
      contracts,
      cleanupAddress,
      signAndEnqueueTxMock,
    );
    await invokeOnSuffice(action, [fraud, fraud], unspentBoxes);

    expect(signAndEnqueueTxMock).toHaveBeenCalledTimes(2);
  });
});
