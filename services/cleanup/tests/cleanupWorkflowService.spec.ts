import { describe, expect, it, vi } from 'vitest';
import { DataSource } from '@rosen-bridge/extended-typeorm';
import JsonBigInt from '@rosen-bridge/json-bigint';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { CommitmentEntity } from '@rosen-bridge/watcher-data-extractor';
import { BoxEntity } from '@rosen-bridge/address-extractor';
import { FraudEntity } from '@rosen-bridge/fraud-extractor';
import { CollateralEntity, EventTriggerEntity } from '@rosen-bridge/watcher-data-extractor';
import { TransactionEntity } from '@rosen-bridge/tx-pot';
import { OutputBox } from '@ergo-raffle/box-lookup';

import { DBService } from '../src/services/dbService';
import { TxPotService } from '../src/services/txPotService';
import { BoxLookupService } from '../src/services/boxLookupService';
import { CleanupWorkflowService } from '../src/services/cleanupWorkflowService';
import { ScannerService } from '../src/services/scannerService';
import { resetServiceInstance } from './testUtils';
import { boxLookupRegisterRequestMock } from './mocked/BoxLookup.mock';
import { txPotAddTxMock, txPotGetTxsQueryMock } from './mocked/TxPot.mock';
import {
  workflowCollateralOutputBox,
  workflowFraudCleanerErgoBoxJson,
  workflowFraudFeeErgoBoxesJson,
  workflowFraudOutputBox,
  workflowContracts,
  workflowMockRepoWids,
  workflowRepoOutputBox,
  workflowTriggerEventOutputBox,
} from './testData';

const testEntities = [
  TransactionEntity,
  BoxEntity,
  EventTriggerEntity,
  FraudEntity,
  CollateralEntity,
  CommitmentEntity,
];

describe('startService', () => {
  /**
   * @target should register trigger and fraud box-lookup requests on start
   * @dependencies
   * - DBService started
   * - TxPotService started
   * - BoxLookupService started
   * @scenario
   * - Start CleanupWorkflowService
   * @expected
   * - BoxLookup.registerRequest called twice
   */
  it('should register trigger and fraud box-lookup requests on start', async () => {
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
    resetServiceInstance(CleanupWorkflowService);

    DBService.init(ds);
    await DBService.getInstance().startService();
    ScannerService.init(
      1,
      'http://127.0.0.1:9052',
      'http://explorer',
      0,
      ergoLib.NetworkPrefix.Mainnet,
      workflowContracts,
      [],
      undefined,
    );
    await ScannerService.getInstance().startService();
    TxPotService.init(1, ds, 10);
    await TxPotService.getInstance().startService();

    BoxLookupService.init(1, 'http://127.0.0.1:9052', undefined);
    await BoxLookupService.getInstance().startService();

    CleanupWorkflowService.init();
    const ok = await CleanupWorkflowService.getInstance().startService();

    expect(ok).toBe(true);
    expect(boxLookupRegisterRequestMock).toHaveBeenCalledTimes(4);

    await ds.destroy();
    vi.useRealTimers();
  });
});

describe('getConfirmedBoxes', () => {
  /**
   * @target should filter trigger boxes to only expired ones
   * @dependencies
   * - CleanupWorkflowService started
   * @scenario
   * - Start CleanupWorkflowService
   * - Call trigger request getConfirmedBoxes
   * @expected
   * - Only boxes with creationHeight <= (currentHeight - cleanupConfirm) are returned
   */
  it('should filter trigger boxes to only expired ones', async () => {
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
    resetServiceInstance(CleanupWorkflowService);
    boxLookupRegisterRequestMock.mockClear();

    DBService.init(ds);
    await DBService.getInstance().startService();
    ScannerService.init(
      1,
      'http://127.0.0.1:9052',
      'http://explorer',
      0,
      ergoLib.NetworkPrefix.Mainnet,
      workflowContracts,
      [],
      undefined,
    );
    await ScannerService.getInstance().startService();
    vi.spyOn(ScannerService.getInstance(), 'getCurrentHeight').mockResolvedValue(2000000);
    const nonExpired = { ...workflowTriggerEventOutputBox, creationHeight: 2000001 };
    const expired = { ...workflowTriggerEventOutputBox, boxId: `${workflowTriggerEventOutputBox.boxId}-2`, creationHeight: 0 };
    vi.spyOn(ScannerService.getInstance(), 'getUnspentTriggerBoxes').mockResolvedValue([nonExpired, expired]);

    TxPotService.init(1, ds, 10);
    await TxPotService.getInstance().startService();
    BoxLookupService.init(1, 'http://127.0.0.1:9052', undefined);
    await BoxLookupService.getInstance().startService();

    CleanupWorkflowService.init();
    await CleanupWorkflowService.getInstance().startService();

    const triggerReq = boxLookupRegisterRequestMock.mock.calls[2]?.[0];
    if (!triggerReq) throw new Error('trigger request not registered');

    const confirmed = await triggerReq.getConfirmedBoxes();
    expect(confirmed.map((b) => b.boxId)).toEqual([expired.boxId]);

    await ds.destroy();
    vi.useRealTimers();
  });
});

describe('onSuffice', () => {
  /**
   * @target should enqueue approved fraud tx when trigger request suffices
   * @dependencies
   * - Same as startService
   * @scenario
   * - Start CleanupWorkflowService
   * - Invoke trigger request onSuffice callback
   * @expected
   * - TxPot.addTx called once with txType 'fraud'
   */
  it('should enqueue approved fraud tx when trigger request suffices', async () => {
    vi.useFakeTimers();

    const fraudCleanerBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(workflowFraudCleanerErgoBoxJson),
    );
    const triggerBox = workflowTriggerEventOutputBox;

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
    resetServiceInstance(CleanupWorkflowService);
    boxLookupRegisterRequestMock.mockClear();

    DBService.init(ds);
    await DBService.getInstance().startService();
    ScannerService.init(
      1,
      'http://127.0.0.1:9052',
      'http://explorer',
      0,
      ergoLib.NetworkPrefix.Mainnet,
      workflowContracts,
      [],
      undefined,
    );
    await ScannerService.getInstance().startService();
    vi.spyOn(ScannerService.getInstance(), 'getCurrentHeight').mockResolvedValue(2000000);
    vi.spyOn(ScannerService.getInstance(), 'getUnspentBoxesByAddress').mockResolvedValue([]);
    TxPotService.init(1, ds, 10);
    await TxPotService.getInstance().startService();

    BoxLookupService.init(1, 'http://127.0.0.1:9052', undefined);
    await BoxLookupService.getInstance().startService();

    CleanupWorkflowService.init();
    await CleanupWorkflowService.getInstance().startService();

    const cleanupReq = boxLookupRegisterRequestMock.mock.calls[0]?.[0];
    const repoReq = boxLookupRegisterRequestMock.mock.calls[1]?.[0];
    const triggerReq = boxLookupRegisterRequestMock.mock.calls[2]?.[0];
    if (!cleanupReq || !repoReq || !triggerReq) throw new Error('request not registered');

    txPotGetTxsQueryMock.mockResolvedValueOnce([]);
    await cleanupReq.onSuffice(
      [
        workflowFraudCleanerErgoBoxJson as OutputBox,
        {
          ...workflowFraudFeeErgoBoxesJson[0]!,
          value: String(workflowFraudFeeErgoBoxesJson[0]!.value),
        } as OutputBox,
      ],
      [],
      0,
    );

    await repoReq.onSuffice([workflowRepoOutputBox], [], 0);

    // Populate DB with commitments spent by the trigger tx (order must match spendIndex).
    await ds.getRepository(CommitmentEntity).insert(
      workflowMockRepoWids.map((wid, i) => ({
        txId: 't',
        extractor: 'commitment-extractor',
        eventId: 'e',
        commitment: 'c',
        WID: wid,
        boxId: `box-${i}`,
        block: 'b',
        height: 1,
        boxSerialized: 's',
        spendTxId: triggerBox.transactionId,
        spendIndex: i,
      })),
    );

    await triggerReq.onSuffice([triggerBox], [], 1);

    expect(txPotAddTxMock).toHaveBeenCalledTimes(1);
    expect(txPotAddTxMock.mock.calls[0]?.[2]).toBe('fraud');

    await ds.destroy();
    vi.useRealTimers();
  });

  /**
   * @target should enqueue approved slash tx when fraud request suffices
   * @dependencies
   * - Same as startService
   * @scenario
   * - Start CleanupWorkflowService
   * - Invoke fraud request onSuffice callback
   * @expected
   * - TxPot.addTx called once with txType 'slash'
   */
  it('should enqueue approved slash tx when fraud request suffices', async () => {
    vi.useFakeTimers();

    const slCleanupBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(workflowFraudCleanerErgoBoxJson),
    );
    const slRepoBox = workflowRepoOutputBox;
    const slCollateralBox = workflowCollateralOutputBox;
    const slFraudBox = workflowFraudOutputBox;

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
    resetServiceInstance(CleanupWorkflowService);
    boxLookupRegisterRequestMock.mockClear();

    DBService.init(ds);
    await DBService.getInstance().startService();
    ScannerService.init(
      1,
      'http://127.0.0.1:9052',
      'http://explorer',
      0,
      ergoLib.NetworkPrefix.Mainnet,
      workflowContracts,
      [],
      undefined,
    );
    await ScannerService.getInstance().startService();
    vi.spyOn(ScannerService.getInstance(), 'getCurrentHeight').mockResolvedValue(2000000);
    vi.spyOn(ScannerService.getInstance(), 'getUnspentBoxesByAddress').mockResolvedValue([]);
    vi.spyOn(ScannerService.getInstance(), 'getUnspentCollateralBoxes').mockResolvedValue([slCollateralBox]);
    TxPotService.init(1, ds, 10);
    await TxPotService.getInstance().startService();

    BoxLookupService.init(1, 'http://127.0.0.1:9052', undefined);
    await BoxLookupService.getInstance().startService();

    CleanupWorkflowService.init();
    await CleanupWorkflowService.getInstance().startService();

    const cleanupReq = boxLookupRegisterRequestMock.mock.calls[0]?.[0];
    const repoReq = boxLookupRegisterRequestMock.mock.calls[1]?.[0];
    const fraudReq = boxLookupRegisterRequestMock.mock.calls[3]?.[0];
    if (!cleanupReq || !repoReq || !fraudReq) throw new Error('request not registered');

    txPotAddTxMock.mockClear();
    txPotGetTxsQueryMock.mockResolvedValueOnce([]);
    await cleanupReq.onSuffice(
      [
        workflowFraudCleanerErgoBoxJson as OutputBox,
        {
          ...workflowFraudFeeErgoBoxesJson[0]!,
          value: String(workflowFraudFeeErgoBoxesJson[0]!.value),
        } as OutputBox,
      ],
      [],
      0,
    );
    await repoReq.onSuffice([workflowRepoOutputBox as OutputBox], [], 0);
    await fraudReq.onSuffice([slFraudBox], [], 2);

    // A collateral request should be registered dynamically.
    const collateralReq = boxLookupRegisterRequestMock.mock.calls[4]?.[0];
    if (!collateralReq) throw new Error('collateral request not registered');
    await collateralReq.onSuffice([slCollateralBox as OutputBox], [], 3);

    expect(txPotAddTxMock).toHaveBeenCalledTimes(1);
    expect(txPotAddTxMock.mock.calls[0]?.[2]).toBe('slash');

    await ds.destroy();
    vi.useRealTimers();
  });
});


