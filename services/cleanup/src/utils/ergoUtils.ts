import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { hasToken } from './cleanupUtils';
import { OutputBox } from '@ergo-raffle/box-lookup';

/**
 * Derives a wasm `SecretKey` from a BIP-39 mnemonic phrase.
 *
 * @param mnemonic - Mnemonic phrase
 * @returns Secret key
 */
export const mnemonicToSecretKey = (mnemonic: string): ergoLib.SecretKey => {
  const seed = ergoLib.Mnemonic.to_seed(mnemonic, '');
  const rootSecret = ergoLib.ExtSecretKey.derive_master(seed);
  // const changePath = ergoLib.DerivationPath.new(0, new Uint32Array([0]));
  const changePath = ergoLib.DerivationPath.from_string("m/44'/429'/0'/0/0");
  const secretKeyBytes = rootSecret.derive(changePath).secret_key_bytes();
  return ergoLib.SecretKey.dlog_from_bytes(secretKeyBytes);
};

/**
 * Derives the cleanup service address from mnemonic.
 *
 * @param mnemonic - Mnemonic phrase
 * @param networkPrefix - Network prefix (mainnet/testnet)
 * @returns Base58 address
 */
export const mnemonicToAddress = (
  mnemonic: string,
  networkPrefix: ergoLib.NetworkPrefix,
): string => {
  return mnemonicToSecretKey(mnemonic).get_address().to_base58(networkPrefix);
};

export const signTx = async (
  ctx: ergoLib.ErgoStateContext,
  mnemonic: string,
  unsignedTx: ergoLib.UnsignedTransaction,
  inputBoxes: ergoLib.ErgoBox[],
  dataInputs: ergoLib.ErgoBox[] = []
): Promise<ergoLib.Transaction> => {
  const secrets = new ergoLib.SecretKeys();
  secrets.add(mnemonicToSecretKey(mnemonic));
  const wallet = ergoLib.Wallet.from_secrets(secrets);

  const inputs = ergoLib.ErgoBoxes.empty();
  inputBoxes.forEach((b) => inputs.add(b));
  const data = ergoLib.ErgoBoxes.empty();
  dataInputs.forEach((b) => data.add(b));

  return wallet.sign_transaction(ctx, unsignedTx, inputs, data);
};

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


/**
 * Returns the collateral box from a signed transaction outputs.
 *
 * @param tx - Signed transaction
 * @param awcNftTokenId - AWC NFT token id
 * @returns Collateral box
 * @throws When collateral box is not found in outputs
 */
export const getCollateralFromTx = (
  tx: ergoLib.Transaction,
  awcNftTokenId: string,
): OutputBox => {
  const outputs = getOutputs(tx);
  const collateral = outputs.find((b) => hasToken(b, awcNftTokenId));
  if (!collateral) throw new Error('collateral box not found in tx outputs');
  return collateral.to_js_eip12() as OutputBox;
};