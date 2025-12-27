import * as ergoLib from 'ergo-lib-wasm-nodejs';

import { mnemonicToSecretKey } from './keys';
import { ErgoNetwork } from './ergoNetwork';

export const signTx = async (
  nodeUrl: string,
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

  const ctx = await ErgoNetwork.getErgoStateContext(nodeUrl);
  return wallet.sign_transaction(ctx, unsignedTx, inputs, data);
};


