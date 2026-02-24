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
 * Extracts a single WID from R4 when it is encoded as `Coll[Byte]`.
 *
 * @param box - Input box
 * @returns WID as a hex string
 */
export const getWidFromR4Bytes = (box: ergoLib.ErgoBox): string => {
  return getRegisterBytesHex(box, ergoLib.NonMandatoryRegisterId.R4, 'R4');
};

/**
 * Returns the amount of a token inside a box.
 *
 * @param box - Input box
 * @param tokenId - Token id to look up
 * @returns Token amount
 * @throws When token does not exist in the box
 */
export const getTokenAmount = (
  box: ergoLib.ErgoBox,
  tokenId: string,
): bigint => {
  const tokens = box.tokens();
  for (let i = 0; i < tokens.len(); i++) {
    const token = tokens.get(i);
    if (token.id().to_str() === tokenId) {
      return BigInt(token.amount().as_i64().to_str());
    }
  }
  throw new Error(`Token ${tokenId} not found in box ${box.box_id().to_str()}`);
};
