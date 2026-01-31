import { vi } from 'vitest';
import { CleanupWorkflowService } from '../../src/services/cleanupWorkflowService';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { OutputBox } from '@ergo-raffle/box-lookup';
import { CleanupTxType, RosenContracts } from '../../src/types';

export const spyOnSignAndEnqueueTx = (serviceInstance: CleanupWorkflowService) => {
  return vi
    .spyOn(
      serviceInstance as unknown as Record<string, () => Promise<ergoLib.Transaction>>,
      'signAndEnqueueTx',
    )
    .mockResolvedValue({} as unknown as ergoLib.Transaction);
};

type CleanupWorkflowServicePrivates = {
  signAndEnqueueTx: (
    txType: CleanupTxType,
    unsignedTx: ergoLib.UnsignedTransaction,
    inputBoxes: ergoLib.ErgoBox[],
    height: number,
  ) => Promise<ergoLib.Transaction>;
  onCollateralSuffice: (
    fraud: OutputBox,
    fraudBox: ergoLib.ErgoBox,
    wid: string,
    contracts: RosenContracts,
    cleanupAddress: string,
    boxes: OutputBox[],
  ) => Promise<void>;
  registerCollateralRequest: (
    fraud: OutputBox,
    fraudBox: ergoLib.ErgoBox,
    wid: string,
    contracts: RosenContracts,
    cleanupAddress: string,
  ) => number;
  state: {
    pendingCollateralRequestsByWid: Map<string, number>;
    fraudQueueByWid: Map<string, OutputBox[]>;
    cleanupCache?: { cleanupBox: ergoLib.ErgoBox; feeBoxes: ergoLib.ErgoBox[] };
    repoBoxCache?: OutputBox;
  };
  contracts?: RosenContracts;
  cleanupAddress?: string;
};

export const spyOnRegisterCollateralRequest = (serviceInstance: CleanupWorkflowService) => {
  return vi.spyOn(
    serviceInstance as unknown as CleanupWorkflowServicePrivates,
    'registerCollateralRequest',
  );
};

export const getWorkflowContracts = (serviceInstance: CleanupWorkflowService) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).contracts;
};

export const getCleanupAddress = (serviceInstance: CleanupWorkflowService) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).cleanupAddress;
};

export const getCleanupCache = (serviceInstance: CleanupWorkflowService) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).state.cleanupCache;
};

export const getRepoBoxCache = (serviceInstance: CleanupWorkflowService) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).state.repoBoxCache;
};

export const callSignAndEnqueueTx = async (
  serviceInstance: CleanupWorkflowService,
  txType: CleanupTxType,
  unsignedTx: ergoLib.UnsignedTransaction,
  inputBoxes: ergoLib.ErgoBox[],
  height: number,
) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).signAndEnqueueTx(
    txType,
    unsignedTx,
    inputBoxes,
    height,
  );
};

export const setCleanupCache = (
  serviceInstance: CleanupWorkflowService,
  cleanupBox: ergoLib.ErgoBox,
  feeBoxes: ergoLib.ErgoBox[],
) => {
  (serviceInstance as unknown as CleanupWorkflowServicePrivates).state.cleanupCache = { cleanupBox, feeBoxes };
};

export const setFraudQueueByWid = (serviceInstance: CleanupWorkflowService, wid: string, frauds: OutputBox[]) => {
  (serviceInstance as unknown as CleanupWorkflowServicePrivates).state.fraudQueueByWid.set(wid, frauds);
};

export const setRepoBoxCache = (serviceInstance: CleanupWorkflowService, repoBox: OutputBox) => {
  (serviceInstance as unknown as CleanupWorkflowServicePrivates).state.repoBoxCache = repoBox;
};
