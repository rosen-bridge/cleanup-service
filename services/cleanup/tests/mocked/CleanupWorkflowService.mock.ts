import { vi } from 'vitest';
import { CleanupWorkflowService } from '../../src/services/cleanupWorkflowService';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { OutputBox } from '@ergo-raffle/box-lookup';
import { CleanupTxType, RosenContracts } from '../../src/types';

export const spyOnSignAndEnqueueTx = (serviceInstance: CleanupWorkflowService) => {
  return vi.spyOn(serviceInstance as unknown as Record<string, () => Promise<void>>, 'signAndEnqueueTx').mockResolvedValue(undefined);
};

type CleanupWorkflowServicePrivates = {
  signAndEnqueueTx: (
    txType: CleanupTxType,
    unsignedTx: ergoLib.UnsignedTransaction,
    inputBoxes: ergoLib.ErgoBox[],
    height: number,
    extra: string,
    extra2: string,
  ) => Promise<void>;
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
    contracts: RosenContracts,
    cleanupAddress: string,
  ) => number;
  pendingCollateralRequests: Map<string, number>;
  cleanupCache?: { cleanupBox: ergoLib.ErgoBox; feeBoxes: ergoLib.ErgoBox[] };
  repoBoxCache?: OutputBox;
  contracts?: RosenContracts;
  cleanupAddress?: string;
};

export const spyOnRegisterCollateralRequest = (serviceInstance: CleanupWorkflowService) => {
  return vi.spyOn(
    serviceInstance as unknown as CleanupWorkflowServicePrivates,
    'registerCollateralRequest',
  );
};

export const getPendingCollateralRequests = (serviceInstance: CleanupWorkflowService) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).pendingCollateralRequests;
};

export const getWorkflowContracts = (serviceInstance: CleanupWorkflowService) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).contracts;
};

export const getCleanupAddress = (serviceInstance: CleanupWorkflowService) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).cleanupAddress;
};

export const getCleanupCache = (serviceInstance: CleanupWorkflowService) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).cleanupCache;
};

export const getRepoBoxCache = (serviceInstance: CleanupWorkflowService) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).repoBoxCache;
};

export const callRegisterCollateralRequest = (
  serviceInstance: CleanupWorkflowService,
  fraud: OutputBox,
  fraudBox: ergoLib.ErgoBox,
  contracts: RosenContracts,
  cleanupAddress: string,
) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).registerCollateralRequest(
    fraud,
    fraudBox,
    contracts,
    cleanupAddress,
  );
};

export const callOnCollateralSuffice = async (
  serviceInstance: CleanupWorkflowService,
  fraud: OutputBox,
  fraudBox: ergoLib.ErgoBox,
  wid: string,
  contracts: RosenContracts,
  cleanupAddress: string,
  boxes: OutputBox[],
) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).onCollateralSuffice(
    fraud,
    fraudBox,
    wid,
    contracts,
    cleanupAddress,
    boxes,
  );
};

export const callSignAndEnqueueTx = async (
  serviceInstance: CleanupWorkflowService,
  txType: CleanupTxType,
  unsignedTx: ergoLib.UnsignedTransaction,
  inputBoxes: ergoLib.ErgoBox[],
  height: number,
  extra: string,
  extra2: string,
) => {
  return (serviceInstance as unknown as CleanupWorkflowServicePrivates).signAndEnqueueTx(
    txType,
    unsignedTx,
    inputBoxes,
    height,
    extra,
    extra2,
  );
};

export const setCleanupCache = (
  serviceInstance: CleanupWorkflowService,
  cleanupBox: ergoLib.ErgoBox,
  feeBoxes: ergoLib.ErgoBox[],
) => {
  (serviceInstance as unknown as CleanupWorkflowServicePrivates).cleanupCache = { cleanupBox, feeBoxes };
};

export const setRepoBoxCache = (serviceInstance: CleanupWorkflowService, repoBox: OutputBox) => {
  (serviceInstance as unknown as CleanupWorkflowServicePrivates).repoBoxCache = repoBox;
};
