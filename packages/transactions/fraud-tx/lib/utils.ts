import { ErgoBoxProxy } from '@rosen-bridge/ergo-box-selection';
import * as ergoLib from 'ergo-lib-wasm-nodejs';

/**
 * Converts a hex string to a Uint8Array
 * @param hex - The hex string to convert
 * @returns Uint8Array representation
 */
export const hexToUint8Array = (hex: string): Uint8Array => {
  return Uint8Array.from(Buffer.from(hex, 'hex'));
};

/**
 * Converts a bigint to a Uint8Array (big-endian, 8 bytes)
 * @param value - The bigint value to convert
 * @returns Uint8Array representation
 */
export const bigIntToUint8Array = (value: bigint): Uint8Array => {
  const buffer = new ArrayBuffer(8);
  new DataView(buffer).setBigUint64(0, value);
  return new Uint8Array(buffer);
};

/**
 * Converts an array of ErgoBox to an ErgoBoxProxy generator
 * for use with '@rosen-bridge/ergo-box-selection' library
 * @param boxes - Array of ErgoBox objects
 * @returns Generator of ErgoBoxProxy objects
 */
export function* toErgoBoxProxyIterator(
  boxes: ergoLib.ErgoBox[],
): Generator<ErgoBoxProxy, undefined> {
  for (const box of boxes) {
    yield box.to_js_eip12();
  }
  return undefined;
}
