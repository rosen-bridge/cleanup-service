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


