import * as ergoLib from 'ergo-lib-wasm-nodejs';

/**
 * Extracts a register value from a box.
 *
 * @param box - Input box
 * @param id - Register id
 * @param name - Register name
 * @returns Register value
 */
const getRequiredRegister = (
  box: ergoLib.ErgoBox,
  id: ergoLib.NonMandatoryRegisterId,
  name: string,
): ergoLib.Constant => {
  const register = box.register_value(id);
  if (!register) throw new Error(`${name} register is missing`);
  return register;
};

/**
 * Extracts the bytes of a register as a hex string.
 *
 * @param box - Input box
 * @param id - Register id
 * @param name - Register name
 * @returns Register bytes as a hex string
 */
export const getRegisterBytesHex = (
  box: ergoLib.ErgoBox,
  id: ergoLib.NonMandatoryRegisterId,
  name: string,
): string =>
  Buffer.from(getRequiredRegister(box, id, name).to_byte_array()).toString(
    'hex',
  );


/**
 * Converts a hex string to a Uint8Array
 *
 * @param hex - Hex string to convert
 * @returns Uint8Array representation
 */
export const hexToUint8Array = (hex: string): Uint8Array => {
  return new Uint8Array(Buffer.from(hex, 'hex'));
};
