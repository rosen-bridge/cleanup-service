import * as ergoLib from 'ergo-lib-wasm-nodejs';

/**
 * Represents a trigger event box containing watcher IDs that need to be frauded
 */
export interface TriggerEventData {
  box: ergoLib.ErgoBox;
  wids: string[]; // Array of watcher ID hex strings
  rwtAmount: bigint; // Total RWT tokens in the event box
}

/**
 * Result of building an unsigned fraud transaction
 */
export interface FraudTransactionBuildResult {
  unsignedTx: ergoLib.UnsignedTransaction;
  inputBoxes: ergoLib.ErgoBox[];
}
