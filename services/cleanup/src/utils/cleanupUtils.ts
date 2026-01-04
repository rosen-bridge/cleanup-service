import { OutputBox } from '@ergo-raffle/box-lookup';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { RWTRepoData } from '@rosen-bridge/slash-tx';
import { CleanupTokenIds } from '../types/cleanup';
import { TransactionEntity } from '@rosen-bridge/tx-pot';
import { DeserializedTx } from '@ergo-raffle/box-lookup';
/**
 * Converts a Uint8Array to a hex string.
 *
 * @param bytes - Uint8Array to convert
 * @returns Hex string
 */
const uint8ArrayToHex = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString('hex');

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
): string => uint8ArrayToHex(getRequiredRegister(box, id, name).to_byte_array());

/**
 * Converts a `box-lookup` `OutputBox` to a wasm `ErgoBox` by normalizing numeric fields
 * and parsing via `ErgoBox.from_json`.
 *
 * @param box - Plain output box returned by box-lookup
 * @returns Parsed wasm `ErgoBox`
 */
export const outputBoxToErgoBox = (box: OutputBox): ergoLib.ErgoBox => {
  return ergoLib.ErgoBox.from_json(
    JSON.stringify({
      ...box,
      value: String(box.value),
      assets: box.assets.map((a) => ({
        ...a,
        amount: String(a.amount),
      })),
    }),
  );
};

/**
 * Returns the amount of a token inside a box.
 *
 * @param box - Input box
 * @param tokenId - Token id to look up
 * @returns Token amount
 * @throws When token does not exist in the box
 */
export const getTokenAmount = (box: ergoLib.ErgoBox, tokenId: string): bigint => {
  const tokens = box.tokens();
  for (let i = 0; i < tokens.len(); i++) {
    const token = tokens.get(i);
    if (token.id().to_str() === tokenId) {
      return BigInt(token.amount().as_i64().to_str());
    }
  }
  throw new Error(`Token ${tokenId} not found in box ${box.box_id().to_str()}`);
};

/**
 * Checks whether a token exists in the box.
 *
 * @param box - Input box
 * @param tokenId - Token id to look up
 * @returns True when token exists
 */
export const hasToken = (box: ergoLib.ErgoBox, tokenId: string): boolean => {
  const tokens = box.tokens();
  for (let i = 0; i < tokens.len(); i++) {
    if (tokens.get(i).id().to_str() === tokenId) return true;
  }
  return false;
};

/**
 * Extracts trigger WID-list digest from R4 where it is encoded as `Coll[Byte]`.
 *
 * Canonical format (watcher / contracts repo):
 * - Trigger R4: `Coll[Byte]` = blake2b256(concat(WIDs))
 *
 * @param box - Input box
 * @returns WID list digest as hex string
 */
export const getWidListDigestFromR4 = (box: ergoLib.ErgoBox): string => {
  return getRegisterBytesHex(box, ergoLib.NonMandatoryRegisterId.R4, 'R4');
};

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
 * Finds a watcher collateral box by WID.
 *
 * @param boxes - Unspent boxes at collateral address
 * @param awcNftTokenId - AWC NFT token id (must be first token)
 * @param wid - WID hex
 * @returns Collateral box
 */
export const findCollateralBoxByWid = (
  boxes: OutputBox[],
  awcNftTokenId: string,
  wid: string,
): OutputBox[] => {
  const candidates = boxes.filter(
    (b) => b.assets.some((asset) => asset.tokenId === awcNftTokenId),
  );
  const result = [];
  for (const c of candidates) {
    const parsed = outputBoxToErgoBox(c);
    if (getWidFromR4Bytes(parsed) === wid) result.push(c);
  }
  return result;
};

/**
 * Extracts commitment count from R7 where it is encoded as `Int`.
 *
 * Canonical format (watcher / contracts repo):
 * - Trigger R7: `Int` = number of commitments consumed for trigger creation
 */
export const getCommitmentCountFromR7 = (box: ergoLib.ErgoBox): number => {
  return getRequiredRegister(box, ergoLib.NonMandatoryRegisterId.R7, 'R7').to_i32();
};

/**
 * Extracts RSN amount from R5 where it is encoded as `Long`.
 *
 * @param box - Input box
 * @returns RSN amount
 */
export const getRsnAmountFromR5 = (box: ergoLib.ErgoBox): bigint => {
  return BigInt(
    getRequiredRegister(box, ergoLib.NonMandatoryRegisterId.R5, 'R5').to_i64().to_str(),
  );
};

/**
 * Maps an RWT repo `OutputBox` into `slash-tx` builder input.
 *
 * @param repoBox - Repo box
 * @param tokenIds - Required token ids for repo validation
 * @returns `RWTRepoData` for slash-tx
 */
export const toRwtRepoData = (repoBox: OutputBox, tokenIds: CleanupTokenIds): RWTRepoData => {
  const box = outputBoxToErgoBox(repoBox);
  getTokenAmount(box, tokenIds.repoNftTokenId);
  getTokenAmount(box, tokenIds.rwtTokenId);
  getTokenAmount(box, tokenIds.rsnTokenId);
  getTokenAmount(box, tokenIds.awcTokenId);

  return {
    box,
    repoNFT: tokenIds.repoNftTokenId,
    rwtTokenId: tokenIds.rwtTokenId,
    rsnTokenId: tokenIds.rsnTokenId,
    awcTokenId: tokenIds.awcTokenId,
  };
};

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

