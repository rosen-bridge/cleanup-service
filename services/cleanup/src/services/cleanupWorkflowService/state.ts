import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { OutputBox } from '@ergo-raffle/box-lookup';

import { RequestIds } from '../../types';

/**
 * Holds mutable workflow state used during box-lookup rounds.
 */
export class CleanupWorkflowState {
  cleanupCache?: { cleanupBox: ergoLib.ErgoBox; feeBoxes: ergoLib.ErgoBox[] };
  repoBoxCache?: OutputBox;
  pendingCollateralRequestsByWid = new Map<string, number>();
  fraudQueueByWid = new Map<string, OutputBox[]>();
  collateralBoxByWid = new Map<string, OutputBox>();
  requestIds: RequestIds = {};

  /**
   * Clears round-scoped caches and queues.
   */
  resetRound = (): void => {
    this.cleanupCache = undefined;
    this.repoBoxCache = undefined;
    this.pendingCollateralRequestsByWid.clear();
    this.fraudQueueByWid.clear();
    this.collateralBoxByWid.clear();
  };

  /**
   * Clears all state including request IDs.
   */
  resetAll = (): void => {
    this.resetRound();
    this.requestIds = {};
  };
}
