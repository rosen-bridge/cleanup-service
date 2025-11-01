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
 * Converts an ErgoBox iterator to an ErgoBoxProxy iterator
 * for use with '@rosen-bridge/ergo-box-selection' library
 * @param boxIterator - Iterator of ErgoBox objects
 * @returns Iterator of ErgoBoxProxy objects
 */
export const toErgoBoxProxyIterator = (
  boxIterator: Iterator<ergoLib.ErgoBox, undefined>,
): Iterator<ErgoBoxProxy, undefined> => {
  return {
    next: (): IteratorResult<ErgoBoxProxy, undefined> => {
      const { value } = boxIterator.next();
      return value !== undefined
        ? {
            value: value.to_js_eip12(),
            done: false,
          }
        : { value: undefined, done: true };
    },
  };
};
