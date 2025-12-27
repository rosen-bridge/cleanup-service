import { OutputBox } from '@ergo-raffle/box-lookup';
import * as ergoLib from 'ergo-lib-wasm-nodejs';

/**
 * Converts base64 sigma-serialized ErgoBox bytes into the plain `box-lookup` `OutputBox` shape.
 *
 * @param serialized - Base64 sigma-serialized ErgoBox bytes
 * @returns OutputBox representation (suitable for box-lookup + tx builders)
 */
export const serializedErgoBoxToOutputBox = (
  serialized: string,
): OutputBox => {
  const bytes = new Uint8Array(Buffer.from(serialized, 'base64'));
  const ergoBox = ergoLib.ErgoBox.sigma_parse_bytes(bytes);
  return ergoBox.to_js_eip12() as OutputBox;
};


