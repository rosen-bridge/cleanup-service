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

/**
 * Creates a change box containing remaining ERG and tokens after accounting for all outputs
 * Calculates: total inputs - total outputs - tx fee = change
 *
 * @param inputBoxes - All input boxes being spent
 * @param outputBoxes - All output boxes being created (excluding change box)
 * @param changeAddress - Address where change should be sent
 * @param txFee - Transaction fee amount
 * @param height - Current blockchain height
 * @returns Change box candidate with remaining assets
 */
export const createChangeBox = (
  inputBoxes: ergoLib.ErgoBox[],
  outputBoxes: ergoLib.ErgoBoxCandidate[],
  changeAddress: string,
  txFee: string,
  height: number,
): ergoLib.ErgoBoxCandidate => {
  // Calculate total input ERG
  let totalInputErg = 0n;
  const inputTokens = new Map<string, bigint>();

  for (const box of inputBoxes) {
    totalInputErg += BigInt(box.value().as_i64().to_str());

    // Aggregate input tokens
    for (let i = 0; i < box.tokens().len(); i++) {
      const token = box.tokens().get(i);
      const tokenId = token.id().to_str();
      const amount = BigInt(token.amount().as_i64().to_str());
      inputTokens.set(tokenId, (inputTokens.get(tokenId) || 0n) + amount);
    }
  }

  // Calculate total output ERG and tokens
  let totalOutputErg = 0n;
  const outputTokens = new Map<string, bigint>();

  for (const box of outputBoxes) {
    totalOutputErg += BigInt(box.value().as_i64().to_str());

    // Aggregate output tokens
    for (let i = 0; i < box.tokens().len(); i++) {
      const token = box.tokens().get(i);
      const tokenId = token.id().to_str();
      const amount = BigInt(token.amount().as_i64().to_str());
      outputTokens.set(tokenId, (outputTokens.get(tokenId) || 0n) + amount);
    }
  }

  // Calculate change ERG: inputs - outputs - fee
  const changeErg = totalInputErg - totalOutputErg - BigInt(txFee);

  // Calculate change tokens: inputs - outputs
  const changeTokens = new Map<string, bigint>();
  inputTokens.forEach((amount, tokenId) => {
    const outputAmount = outputTokens.get(tokenId) || 0n;
    const change = amount - outputAmount;
    if (change > 0n) {
      changeTokens.set(tokenId, change);
    }
  });

  // Build change box
  const boxBuilder = new ergoLib.ErgoBoxCandidateBuilder(
    ergoLib.BoxValue.from_i64(ergoLib.I64.from_str(changeErg.toString())),
    ergoLib.Contract.pay_to_address(
      ergoLib.Address.from_base58(changeAddress),
    ),
    height,
  );

  // Add change tokens
  changeTokens.forEach((amount, tokenId) => {
    boxBuilder.add_token(
      ergoLib.TokenId.from_str(tokenId),
      ergoLib.TokenAmount.from_i64(ergoLib.I64.from_str(amount.toString())),
    );
  });

  return boxBuilder.build();
};
