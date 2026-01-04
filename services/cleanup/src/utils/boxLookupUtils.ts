import { Asset, Request } from '@ergo-raffle/box-lookup';
import { TransactionEntity } from '@rosen-bridge/tx-pot';
import { DeserializedTx } from '@ergo-raffle/box-lookup';
import * as ergoLib from 'ergo-lib-wasm-nodejs';

/**
 * Parses base64-encoded Ergo tx bytes and returns its EIP-12 JSON projection.
 */
export const deserializeTxForBoxLookup = (tx: TransactionEntity): DeserializedTx => {
  const bytes = Uint8Array.from(Buffer.from(tx.serializedTx, 'base64'));
  const parsedTx = ergoLib.Transaction.sigma_parse_bytes(bytes);
  return parsedTx.to_js_eip12();
};


/**
 * Creates a `box-lookup` request for trigger-event boxes.
 *
 * @param ergoTree - Box ergoTree filter
 * @param value - Optional minimal value filter
 * @param tokens - Required tokens filter
 * @param getConfirmedBoxes - Provider for confirmed boxes
 * @param onSuffice - Callback invoked when preferred condition occurs
 * @returns Request definition for BoxLookup
 */
export const createTriggerEventRequest = (
  ergoTree: string,
  value: bigint | undefined,
  tokens: Asset[],
  getConfirmedBoxes: Request['getConfirmedBoxes'],
  onSuffice: Request['onSuffice'],
): Request => {
  return { ergoTree, value, tokens, getConfirmedBoxes, onSuffice };
};

/**
 * Creates a `box-lookup` request for fraud boxes.
 *
 * @param ergoTree - Box ergoTree filter
 * @param value - Optional minimal value filter
 * @param tokens - Required tokens filter
 * @param getConfirmedBoxes - Provider for confirmed boxes
 * @param onSuffice - Callback invoked when preferred condition occurs
 * @returns Request definition for BoxLookup
 */
export const createFraudBoxRequest = (
  ergoTree: string,
  value: bigint | undefined,
  tokens: Asset[],
  getConfirmedBoxes: Request['getConfirmedBoxes'],
  onSuffice: Request['onSuffice'],
): Request => {
  return { ergoTree, value, tokens, getConfirmedBoxes, onSuffice };
};

/**
 * Creates a `box-lookup` request for cleanup boxes + fee boxes at the cleanup address.
 *
 * The request should include the CleanupNFT and enough ERG to cover fee needs, so the
 * returned `boxes` can be split into {cleanupBox, feeBoxes} without custom merging.
 */
export const createCleanupRequest = (
  ergoTree: string,
  value: bigint | undefined,
  tokens: Asset[],
  getConfirmedBoxes: Request['getConfirmedBoxes'],
  onSuffice: Request['onSuffice'],
): Request => {
  return { ergoTree, value, tokens, getConfirmedBoxes, onSuffice };
};

/**
 * Creates a `box-lookup` request for the RWT repo box at the repo address.
 */
export const createRepoRequest = (
  ergoTree: string,
  value: bigint | undefined,
  tokens: Asset[],
  getConfirmedBoxes: Request['getConfirmedBoxes'],
  onSuffice: Request['onSuffice'],
): Request => {
  return { ergoTree, value, tokens, getConfirmedBoxes, onSuffice };
};

/**
 * Creates a `box-lookup` request for the watcher collateral address.
 * Used dynamically per fraud work-item to retrieve the relevant collateral box.
 */
export const createCollateralRequest = (
  ergoTree: string,
  value: bigint | undefined,
  tokens: Asset[],
  getConfirmedBoxes: Request['getConfirmedBoxes'],
  onSuffice: Request['onSuffice'],
): Request => {
  return { ergoTree, value, tokens, getConfirmedBoxes, onSuffice };
};
