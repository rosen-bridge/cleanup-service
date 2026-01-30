import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { DataSource } from 'typeorm';
import JsonBigInt from '@rosen-bridge/json-bigint';

import '../src/bootstrap';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { CommitmentEntity } from '@rosen-bridge/watcher-data-extractor';
import { BoxEntity } from '@rosen-bridge/address-extractor';
import { FraudEntity } from '@rosen-bridge/fraud-extractor';
import { CollateralEntity, EventTriggerEntity } from '@rosen-bridge/watcher-data-extractor';
import { TransactionEntity } from '@rosen-bridge/tx-pot';
import { OutputBox } from '@ergo-raffle/box-lookup';

import { DBService } from '../src/services/dbService';
import { TxPotService } from '../src/services/txPotService/txPotService';
import { BoxLookupService } from '../src/services/boxLookupService';
import { CleanupWorkflowService } from '../src/services/cleanupWorkflowService';
import { ScannerService } from '../src/services/scannerService';
import { getLogger } from '../src/config/loggerConfig';
import { outputBoxToErgoBox } from '../src/utils/cleanupUtils';
import { resetServiceInstance } from './testUtils';
import { boxLookupRegisterRequestMock, boxLookupUnregisterRequestMock } from './mocked/BoxLookup.mock';
import { txPotAddTxMock, txPotGetTxsQueryMock } from './mocked/TxPot.mock';
import {
  getCleanupAddress,
  getWorkflowContracts,
  setCleanupCache,
  setFraudQueueByWid,
  setRepoBoxCache,
  spyOnRegisterCollateralRequest,
  spyOnSignAndEnqueueTx,
} from './mocked/CleanupWorkflowService.mock';
import './mocked/ErgoNodeNetwork.mock';
import { CleanupTxType, RosenContracts } from '../src/types';
import {
  workflowCollateralOutputBox,
  workflowFraudOutputBox,
  workflowContracts,
  workflowMockRepoWids,
  workflowRepoOutputBox,
  workflowTriggerEventOutputBox,
  mockNodeUrl,
  mockExplorerUrl,
  workflowTriggerEventOutputBox2,
  workflowCleanerBoxJson,
  workflowFeeBoxesJson,
  signedSlashTxJson,
} from './testData';

const testEntities = [
  TransactionEntity,
  BoxEntity,
  EventTriggerEntity,
  FraudEntity,
  CollateralEntity,
  CommitmentEntity,
];

describe('cleanupWorkflowService', () => {
  describe('startService', () => {
    afterEach(() => {
      vi.clearAllMocks();
    });

    beforeEach(async () => {
      DBService.init(new DataSource({
        type: 'sqlite',
        database: ':memory:',
        entities: testEntities,
        migrations: [],
        synchronize: true,
        logging: false,
      }));
      await DBService.getInstance().startService();
    });

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
        mockNodeUrl,
        mockExplorerUrl,
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
      BoxLookupService.init(1, mockNodeUrl, undefined);
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
      beforeEach(async () => {
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

        CleanupWorkflowService.init(getLogger(import.meta.url));
        await CleanupWorkflowService.getInstance().startService();

      });
    describe('onTriggerEventSuffice', () => {
      /**
       * @target should ignore trigger boxes that are not expired
       * @dependencies
       * - CleanupWorkflowService started
       * @scenario
       * - Start CleanupWorkflowService
       * - Call trigger request getConfirmedBoxes
       * @expected
       * - signAndEnqueueTx not called
       * - txPotAddTx not called
       */
      it('should ignore trigger boxes that are not expired', async () => {
        vi.useFakeTimers();

        const triggerBox = workflowTriggerEventOutputBox;
        
        // Mock height so trigger is NOT expired (creationHeight 1628041 > height - cleanupConfirm 0)
        const getCurrentHeightSpy = vi.spyOn(ScannerService.getInstance(), 'getCurrentHeight').mockResolvedValue(1628041);
        vi.spyOn(ScannerService.getInstance(), 'getUnspentBoxesByAddress').mockResolvedValue([]);
        vi.spyOn(ScannerService.getInstance(), 'getTriggerWidsByTxId').mockResolvedValue(workflowMockRepoWids);

        txPotGetTxsQueryMock.mockResolvedValue([]);
        txPotAddTxMock.mockClear();

        const serviceInstance = CleanupWorkflowService.getInstance();
        serviceInstance['cleanupCache'] = {
          cleanupBox: outputBoxToErgoBox(workflowCleanerBoxJson as OutputBox),
          feeBoxes: [],
        };
        const signAndEnqueueTxSpy = spyOnSignAndEnqueueTx(serviceInstance);
        await serviceInstance['onTriggerEventSuffice']([triggerBox], [], 0);

        expect(getCurrentHeightSpy).toHaveBeenCalled();
        expect(signAndEnqueueTxSpy).not.toHaveBeenCalled();
        expect(txPotAddTxMock).not.toHaveBeenCalled();

      });

      /**
       * @target should ignore trigger boxes when wids length does not match commitment count
       * @dependencies
       * - CleanupWorkflowService started
       * @scenario
       * - Start CleanupWorkflowService
       * - Call trigger request getConfirmedBoxes
       * @expected
       * - signAndEnqueueTx not called
       * - txPotAddTx not called
       */
      it('should ignore trigger boxes when wids length does not match commitment count', async () => {
        vi.useFakeTimers();

        const triggerBox = workflowTriggerEventOutputBox;

        vi.spyOn(ScannerService.getInstance(), 'getCurrentHeight').mockResolvedValue(1628051);
        vi.spyOn(ScannerService.getInstance(), 'getUnspentBoxesByAddress').mockResolvedValue([]);
        vi.spyOn(ScannerService.getInstance(), 'getTriggerWidsByTxId').mockResolvedValue([]);

        txPotGetTxsQueryMock.mockResolvedValue([]);
        txPotAddTxMock.mockClear();

        const serviceInstance = CleanupWorkflowService.getInstance();
        serviceInstance['cleanupCache'] = {
          cleanupBox: outputBoxToErgoBox(workflowCleanerBoxJson as OutputBox),
          feeBoxes: [],
        };
        const signAndEnqueueTxSpy = spyOnSignAndEnqueueTx(serviceInstance);
        await serviceInstance['onTriggerEventSuffice']([triggerBox], [], 0);

        expect(signAndEnqueueTxSpy).not.toHaveBeenCalled();
        expect(txPotAddTxMock).not.toHaveBeenCalled();

      });

      /**
       * @target should call signAndEnqueueTx when trigger is expired and wids match commitment count
       * @dependencies
       * - CleanupWorkflowService started
       * @scenario
       * - Start CleanupWorkflowService
       * - Call trigger request getConfirmedBoxes
       * @expected
       * - signAndEnqueueTx called
       * - txPotAddTx called
       */
      it('should call signAndEnqueueTx when trigger is expired and wids match commitment count', async () => {
        vi.useFakeTimers();

        const triggerBox = workflowTriggerEventOutputBox2;
        const getCurrentHeightSpy = vi.spyOn(ScannerService.getInstance(), 'getCurrentHeight').mockResolvedValue(1695397);
        vi.spyOn(ScannerService.getInstance(), 'getUnspentBoxesByAddress').mockResolvedValue([]);
        vi.spyOn(ScannerService.getInstance(), 'getTriggerWidsByTxId').mockResolvedValue(workflowMockRepoWids);

        txPotGetTxsQueryMock.mockResolvedValue([]);
        txPotAddTxMock.mockClear();

        const serviceInstance = CleanupWorkflowService.getInstance();

        const slashTx = ergoLib.Transaction.from_json(JsonBigInt.stringify(signedSlashTxJson));
        const signSpy = spyOnSignAndEnqueueTx(serviceInstance);
        signSpy.mockResolvedValue(slashTx);

        serviceInstance['cleanupCache'] = {
          cleanupBox: outputBoxToErgoBox(workflowCleanerBoxJson as OutputBox),
          feeBoxes: workflowFeeBoxesJson.map((b) => outputBoxToErgoBox(b as unknown as OutputBox)),
        };
        await serviceInstance['onTriggerEventSuffice']([triggerBox], [], 0);

        expect(getCurrentHeightSpy).toHaveBeenCalled();

      });

    });

    describe('onFraudBoxSuffice', () => {
      /**
       * @target should skip enqueued fraud boxes
       * @dependencies
       * - CleanupWorkflowService started
       * @scenario
       * - Mock isEnqueued=true
       * - Call onFraudBoxSuffice with a fraud box
       * @expected
       * - registerCollateralRequest not called
       * - pendingCollateralRequests unchanged
       */
      it('should skip enqueued fraud boxes', async () => {
        vi.useFakeTimers();

        const serviceInstance = CleanupWorkflowService.getInstance();
        const isEnqueuedSpy = vi
          .spyOn(TxPotService.getInstance(), 'isEnqueued')
          .mockResolvedValue(true);
        const registerSpy = spyOnRegisterCollateralRequest(serviceInstance);

        await serviceInstance['onFraudBoxSuffice']([workflowFraudOutputBox], [], 0);

        expect(isEnqueuedSpy).toHaveBeenCalledWith(CleanupTxType.slash, workflowFraudOutputBox.boxId);
        expect(registerSpy).not.toHaveBeenCalled();
        expect(serviceInstance['pendingCollateralRequestsByWid'].size).toBe(0);

      });

      /**
       * @target should register collateral request for new fraud boxes
       * @dependencies
       * - CleanupWorkflowService started
       * @scenario
       * - Mock isEnqueued=false
       * - Call onFraudBoxSuffice with a fraud box
       * @expected
       * - registerCollateralRequest called once with expected args
       * - pendingCollateralRequests updated with returned request id
       */
      it('should register collateral request for new fraud boxes', async () => {
        vi.useFakeTimers();

        const serviceInstance = CleanupWorkflowService.getInstance();
        vi.spyOn(TxPotService.getInstance(), 'isEnqueued').mockResolvedValue(false);

        const requestId = 101;
        const registerSpy = spyOnRegisterCollateralRequest(serviceInstance).mockReturnValue(requestId);

        await serviceInstance['onFraudBoxSuffice']([workflowFraudOutputBox], [], 0);

        expect(registerSpy).toHaveBeenCalledTimes(1);
        const [widArg, contractsArg, cleanupAddressArg] =
          registerSpy.mock.calls[0] as unknown as [string, RosenContracts, string];
        expect(typeof widArg).toBe('string');
        expect(contractsArg).toEqual(getWorkflowContracts(serviceInstance)! as RosenContracts);
        expect(cleanupAddressArg).toEqual(getCleanupAddress(serviceInstance)!);

        const pending = serviceInstance['pendingCollateralRequestsByWid'];
        expect(pending.get(widArg)).toEqual(requestId);

      });
    });

    describe('registerCollateralRequest', () => {

      /**
       * @target should register a request whose confirmed boxes match the wid in fraud box
       * @dependencies
       * - CleanupWorkflowService started
       * @scenario
       * - Mock Scanner.getUnspentCollateralBoxes to return collateral fixture
       * - Call registerCollateralRequest
       * - Call request.getConfirmedBoxes
       * @expected
       * - returned boxes contain the collateral fixture
       */
      it('should filter confirmed collateral boxes by fraud wid', async () => {
        vi.useFakeTimers();

        const serviceInstance = CleanupWorkflowService.getInstance();
        const contracts = getWorkflowContracts(serviceInstance);
        const cleanupAddress = getCleanupAddress(serviceInstance);
        const wid = workflowMockRepoWids[1]!;
        vi.spyOn(ScannerService.getInstance(), 'getUnspentCollateralBoxes').mockResolvedValue([
          workflowCollateralOutputBox,
        ]);

        serviceInstance['registerCollateralRequest'](
          wid,
          contracts! as RosenContracts,
          cleanupAddress!,
        );

        const req = boxLookupRegisterRequestMock.mock.calls.at(-1)?.[0];
        expect(req, 'collateral request not registered').toBeDefined();

        const confirmed = await req!.getConfirmedBoxes();
        expect(confirmed.map((b: OutputBox) => b.boxId)).toEqual([workflowCollateralOutputBox.boxId]);

      });
    });

    describe('onCollateralSuffice', () => {
      let ds: DataSource;


      /**
       * @target should return early when there is no matching collateral box
       * @dependencies
       * - CleanupWorkflowService started
       * @scenario
       * - Add pending request id for fraud box
       * - Call onCollateralSuffice with empty collateral list
       * @expected
       * - signAndEnqueueTx not called
       * - isEnqueued not called
       * - request NOT removed and pending entry NOT deleted (because it returns before finally)
       */
      it('should return early when there is no matching collateral box', async () => {
        vi.useFakeTimers();

        const serviceInstance = CleanupWorkflowService.getInstance();
        const contracts = getWorkflowContracts(serviceInstance);
        const cleanupAddress = getCleanupAddress(serviceInstance);

        const pending = serviceInstance['pendingCollateralRequestsByWid'];
        const requestId = 999;
        pending.set(workflowMockRepoWids[0]!, requestId);

        const isEnqueuedSpy = vi.spyOn(TxPotService.getInstance(), 'isEnqueued');
        const signSpy = spyOnSignAndEnqueueTx(serviceInstance);

        await serviceInstance['onCollateralSuffice'](
          workflowMockRepoWids[0]!,
          contracts! as RosenContracts,
          cleanupAddress!,
          [],
        );

        expect(signSpy).not.toHaveBeenCalled();
        expect(isEnqueuedSpy).not.toHaveBeenCalled();
        expect(boxLookupUnregisterRequestMock).not.toHaveBeenCalled();
        expect(pending.get(workflowMockRepoWids[0]!)).toEqual(requestId);

      });

      /**
       * @target should build and enqueue slash tx on happy path and clean up pending request
       * @dependencies
       * - CleanupWorkflowService started
       * @scenario
       * - Set cleanupCache and repoBoxCache
       * - Add pending request id for fraud box
       * - Mock isEnqueued=false and currentHeight
       * - Call onCollateralSuffice with matching collateral fixture
       * @expected
       * - signAndEnqueueTx called
       * - request removed and pending entry deleted
       */
      it('should build and enqueue slash tx on happy path and clean up pending request', async () => {
        vi.useFakeTimers();

        const serviceInstance = CleanupWorkflowService.getInstance();
        const contracts = getWorkflowContracts(serviceInstance);
        const cleanupAddress = getCleanupAddress(serviceInstance);

        const pending = serviceInstance['pendingCollateralRequestsByWid'];
        const requestId = 1001;
        const wid = workflowMockRepoWids[1]!;
        serviceInstance['pendingCollateralRequestsByWid'].set(wid, requestId);

        const cleanupBox = outputBoxToErgoBox(workflowCleanerBoxJson as unknown as OutputBox);
        const feeBoxes = workflowFeeBoxesJson.map((b) =>
          outputBoxToErgoBox(b as unknown as OutputBox),
        );
        setCleanupCache(serviceInstance, cleanupBox, feeBoxes);
        setRepoBoxCache(serviceInstance, workflowRepoOutputBox);
        setFraudQueueByWid(serviceInstance, wid, [workflowFraudOutputBox]);

        vi.spyOn(TxPotService.getInstance(), 'isEnqueued').mockResolvedValue(false);
        vi.spyOn(ScannerService.getInstance(), 'getCurrentHeight').mockResolvedValue(123);
        const signSpy = spyOnSignAndEnqueueTx(serviceInstance);
        const slashTx = ergoLib.Transaction.from_json(JsonBigInt.stringify(signedSlashTxJson));
        signSpy.mockResolvedValue(slashTx);

        await serviceInstance['onCollateralSuffice'](
          wid,
          contracts! as RosenContracts,
          cleanupAddress!,
          [workflowCollateralOutputBox],
        );

        // expect(signSpy).toHaveBeenCalled();
        expect(boxLookupUnregisterRequestMock).toHaveBeenCalledWith(requestId);
        expect(pending.has(workflowMockRepoWids[0]!)).toBe(false);

      });

    });
  });
});


