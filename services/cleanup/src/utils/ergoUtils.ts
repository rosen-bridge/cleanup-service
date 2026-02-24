import * as ergoLib from 'ergo-lib-wasm-nodejs';

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

/**
 * Signs an unsigned transaction with a mnemonic.
 *
 * @param ctx - Ergo state context
 * @param mnemonic - Mnemonic phrase
 * @param unsignedTx - Unsigned transaction
 * @param inputBoxes - Input boxes
 * @param dataInputs - Data inputs
 * @returns Signed transaction
 */
export const signTx = async (
  ctx: ergoLib.ErgoStateContext,
  mnemonic: string,
  unsignedTx: ergoLib.UnsignedTransaction,
  inputBoxes: ergoLib.ErgoBox[],
  dataInputs: ergoLib.ErgoBox[] = [],
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
