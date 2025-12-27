import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { OutputBox } from '@ergo-raffle/box-lookup';
import { hasToken } from '../../utils/cleanup';

/**
 * Returns all outputs of a signed transaction as a JS array.
 *
 * @param tx - Signed transaction
 * @returns Output boxes in order
 */
const getOutputs = (tx: ergoLib.Transaction): ergoLib.ErgoBox[] => {
  const boxes = tx.outputs();
  const out: ergoLib.ErgoBox[] = [];
  for (let i = 0; i < boxes.len(); i++) out.push(boxes.get(i));
  return out;
};

/**
 * Derives the next cleanup box and fee boxes from a signed transaction outputs.
 *
 * - `cleanupBox` is the output containing `cleanupNftTokenId`
 * - `feeBoxes` are the other outputs with the same ergoTree as the cleanup box
 *
 * @param tx - Signed transaction
 * @param cleanupNftTokenId - Cleanup NFT token id
 * @returns Next cleanup cache state
 * @throws When cleanup box is not found in outputs
 */
export const getNextCleanupFromTx = (
  tx: ergoLib.Transaction,
  cleanupNftTokenId: string,
): { cleanupBox: ergoLib.ErgoBox; feeBoxes: ergoLib.ErgoBox[] } => {
  const outputs = getOutputs(tx);
  const cleanupBox = outputs.find((b) => hasToken(b, cleanupNftTokenId));
  if (!cleanupBox) throw new Error('cleanup box not found in tx outputs');

  const cleanupTree = cleanupBox.ergo_tree().to_base16_bytes();
  const feeBoxes = outputs.filter(
    (b) => b.ergo_tree().to_base16_bytes() === cleanupTree && !hasToken(b, cleanupNftTokenId),
  );
  return { cleanupBox, feeBoxes };
};

/**
 * Derives the next repo box from a signed transaction outputs.
 *
 * @param tx - Signed transaction
 * @param repoNftTokenId - Repo NFT token id
 * @returns Repo box in `OutputBox` shape
 * @throws When repo box is not found in outputs
 */
export const getNextRepoFromTx = (
  tx: ergoLib.Transaction,
  repoNftTokenId: string,
): OutputBox => {
  const outputs = getOutputs(tx);
  const repo = outputs.find((b) => hasToken(b, repoNftTokenId));
  if (!repo) throw new Error('repo box not found in tx outputs');
  return repo.to_js_eip12() as OutputBox;
};


