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
import { configs } from '../src/config/config';
import { ERGO_CHAIN_NAME } from '../src/config/constants';
import { outputBoxToErgoBox } from '../src/utils/cleanupUtils';
import * as ergoUtils from '../src/utils/ergoUtils';
import { resetServiceInstance } from './testUtils';
import { boxLookupRegisterRequestMock, boxLookupUnregisterRequestMock } from './mocked/BoxLookup.mock';
import { txPotAddTxMock, txPotGetTxsQueryMock } from './mocked/TxPot.mock';
import {
  callOnCollateralSuffice,
  callRegisterCollateralRequest,
  callSignAndEnqueueTx,
  getCleanupAddress,
  getCleanupCache,
  getPendingCollateralRequests,
  getRepoBoxCache,
  getWorkflowContracts,
  setCleanupCache,
  setRepoBoxCache,
  spyOnRegisterCollateralRequest,
  spyOnSignAndEnqueueTx,
} from './mocked/CleanupWorkflowService.mock';
import './mocked/ErgoNodeNetwork.mock';
import { CleanupTxType } from '../src/types';
import { TransactionStatus } from '@rosen-bridge/tx-pot';
import { dummyUnsignedTx, mkCandidate } from './utils/testUtils';
import {
  slashTxJson,
  workflowCollateralOutputBox,
  workflowCleanupAddress,
  workflowFraudCleanerErgoBoxJson,
  workflowFraudOutputBox,
  workflowContracts,
  workflowMockRepoWids,
  workflowRepoOutputBox,
  workflowSlashFeeErgoBoxesJson,
  workflowSlashCleanupErgoBoxJson,
  workflowTriggerEventOutputBox,
  mockNodeUrl,
  mockExplorerUrl,
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
          cleanupBox: outputBoxToErgoBox(workflowFraudCleanerErgoBoxJson as OutputBox),
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
          cleanupBox: outputBoxToErgoBox(workflowFraudCleanerErgoBoxJson as OutputBox),
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

        const triggerBox = workflowTriggerEventOutputBox;

        const getCurrentHeightSpy = vi.spyOn(ScannerService.getInstance(), 'getCurrentHeight').mockResolvedValue(1628051);
        vi.spyOn(ScannerService.getInstance(), 'getUnspentBoxesByAddress').mockResolvedValue([]);
        vi.spyOn(ScannerService.getInstance(), 'getTriggerWidsByTxId').mockResolvedValue(workflowMockRepoWids);

        txPotGetTxsQueryMock.mockResolvedValue([]);
        txPotAddTxMock.mockClear();

        const serviceInstance = CleanupWorkflowService.getInstance();
        serviceInstance['cleanupCache'] = {
          cleanupBox: outputBoxToErgoBox(workflowFraudCleanerErgoBoxJson as OutputBox),
          feeBoxes: [],
        };
        const signAndEnqueueTxSpy = spyOnSignAndEnqueueTx(serviceInstance);
        await serviceInstance['onTriggerEventSuffice']([triggerBox], [], 0);

        expect(getCurrentHeightSpy).toHaveBeenCalled();
        expect(signAndEnqueueTxSpy).toHaveBeenCalled();

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
        expect(getPendingCollateralRequests(serviceInstance).size).toBe(0);

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
        const [fraudArg, fraudBoxArg, widArg, contractsArg, cleanupAddressArg] =
          registerSpy.mock.calls[0] as unknown as [OutputBox, ergoLib.ErgoBox, string, unknown, string];
        expect(fraudArg).toEqual(workflowFraudOutputBox);
        expect(fraudBoxArg.box_id().to_str()).toEqual(workflowFraudOutputBox.boxId);
        expect(typeof widArg).toBe('string');
        expect(contractsArg).toEqual(getWorkflowContracts(serviceInstance));
        expect(cleanupAddressArg).toEqual(getCleanupAddress(serviceInstance));

        const pending = getPendingCollateralRequests(serviceInstance);
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

        vi.spyOn(ScannerService.getInstance(), 'getUnspentCollateralBoxes').mockResolvedValue([
          workflowCollateralOutputBox,
        ]);

        const fraudBox = outputBoxToErgoBox(workflowFraudOutputBox);
        callRegisterCollateralRequest(
          serviceInstance,
          workflowFraudOutputBox,
          fraudBox,
          workflowMockRepoWids[0]!,
          contracts!,
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

        const pending = getPendingCollateralRequests(serviceInstance);
        const requestId = 999;
        pending.set(workflowMockRepoWids[0]!, requestId);

        const isEnqueuedSpy = vi.spyOn(TxPotService.getInstance(), 'isEnqueued');
        const signSpy = spyOnSignAndEnqueueTx(serviceInstance);

        await callOnCollateralSuffice(
          serviceInstance,
          workflowFraudOutputBox,
          outputBoxToErgoBox(workflowFraudOutputBox),
          workflowMockRepoWids[0]!,
          contracts!,
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

        const pending = getPendingCollateralRequests(serviceInstance);
        const requestId = 1001;
        pending.set(workflowMockRepoWids[0]!, requestId);

        const cleanupBox = outputBoxToErgoBox(workflowSlashCleanupErgoBoxJson as unknown as OutputBox);
        const feeBoxes = workflowSlashFeeErgoBoxesJson.map((b) =>
          outputBoxToErgoBox(b as unknown as OutputBox),
        );
        setCleanupCache(serviceInstance, cleanupBox, feeBoxes);
        setRepoBoxCache(serviceInstance, workflowRepoOutputBox);

        vi.spyOn(TxPotService.getInstance(), 'isEnqueued').mockResolvedValue(false);
        vi.spyOn(ScannerService.getInstance(), 'getCurrentHeight').mockResolvedValue(123);
        const signSpy = spyOnSignAndEnqueueTx(serviceInstance);

        await callOnCollateralSuffice(
          serviceInstance,
          workflowFraudOutputBox,
          outputBoxToErgoBox(workflowFraudOutputBox),
          workflowMockRepoWids[0]!,
          contracts!,
          cleanupAddress!,
          [workflowCollateralOutputBox],
        );

        expect(signSpy).toHaveBeenCalled();
        expect(boxLookupUnregisterRequestMock).toHaveBeenCalledWith(requestId);
        expect(pending.has(workflowMockRepoWids[0]!)).toBe(false);

      });

    describe('signAndEnqueueTx', () => {
      /**
       * @target should sign, enqueue in tx-pot, and update caches for slash tx
       * @dependencies
       * - CleanupWorkflowService started
       * @scenario
       * - Mock ergoUtils.signTx to return a signed slash tx (from testData)
       * - Call signAndEnqueueTx
       * @expected
       * - signTx called with ctx, cleanupMnemonic, unsignedTx, inputBoxes
       * - txPot.addTx called with correct args (id, serialized tx, status)
       * - cleanupCache and repoBoxCache updated based on tx outputs
       */
      it('should sign, addTx, and update caches (slash)', async () => {
        vi.useFakeTimers();

        const serviceInstance = CleanupWorkflowService.getInstance();
        const contracts = getWorkflowContracts(serviceInstance);
        expect(contracts).toBeDefined();
        const signedSlashTx = ergoLib.Transaction.from_json(JsonBigInt.stringify(slashTxJson));

        const unsignedTx = dummyUnsignedTx([mkCandidate(workflowCleanupAddress)]);
        const inputBoxes = [outputBoxToErgoBox(workflowCollateralOutputBox)];

        const signTxSpy = vi.spyOn(ergoUtils, 'signTx').mockResolvedValue(signedSlashTx);
        txPotAddTxMock.mockClear();

        const height = 12345;
        const extra = 'workId';
        const extra2 = 'sourceTxId';
        await callSignAndEnqueueTx(
          serviceInstance,
          CleanupTxType.slash,
          unsignedTx,
          inputBoxes,
          height,
          extra,
          extra2,
        );

        expect(signTxSpy).toHaveBeenCalledTimes(1);
        expect(signTxSpy).toHaveBeenCalledWith(
          expect.anything(),
          configs.workflow.cleanupMnemonic,
          unsignedTx,
          inputBoxes,
        );

        const expectedSerialized = Buffer.from(signedSlashTx.sigma_serialize_bytes()).toString('base64');
        expect(txPotAddTxMock).toHaveBeenCalledWith(
          signedSlashTx.id().to_str(),
          ERGO_CHAIN_NAME,
          CleanupTxType.slash,
          0,
          expectedSerialized,
          TransactionStatus.SIGNED,
          height,
          extra,
          extra2,
        );

        const expectedCleanup = ergoUtils.getNextCleanupFromTx(signedSlashTx, contracts!.tokens.CleanupNFT);
        const expectedRepo = ergoUtils.getNextRepoFromTx(signedSlashTx, contracts!.tokens.RepoNFT);

        const cleanupCache = getCleanupCache(serviceInstance);
        expect(cleanupCache).toBeDefined();
        expect(cleanupCache!.cleanupBox.box_id().to_str()).toEqual(expectedCleanup.cleanupBox.box_id().to_str());
        expect(cleanupCache!.feeBoxes.map((b) => b.box_id().to_str())).toEqual(
          expectedCleanup.feeBoxes.map((b) => b.box_id().to_str()),
        );

        const repoBoxCache = getRepoBoxCache(serviceInstance);
        expect(repoBoxCache).toBeDefined();
        expect(repoBoxCache!.boxId).toEqual(expectedRepo.boxId);
      });
    });
    });
  });
});


