import { Asset, Request } from '@ergo-raffle/box-lookup';

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


