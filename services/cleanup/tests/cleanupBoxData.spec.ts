import { describe, expect, it } from 'vitest';

import {
  outputBoxToErgoBox,
  toCollateralBoxData,
} from '../src/utils/cleanup';
import {
  sampleTxId,
  sampleWids,
  sampleRsnTokenId,
  collateralOutputBox,
  triggerEventOutputBox,
} from './testData';

describe('outputBoxToErgoBox', () => {
  /**
   * @target should parse EIP-12 box into ergo-lib ErgoBox
   * @dependencies
   * - ergo-lib-wasm-nodejs
   * @scenario
   * - Parse OutputBox using outputBoxToErgoBox
   * @expected
   * - Parsed tx id and index match input
   */
  it('should parse EIP-12 box into ergo-lib ErgoBox', () => {
    const parsed = outputBoxToErgoBox(triggerEventOutputBox);
    expect(parsed.tx_id().to_str()).toBe(sampleTxId);
    expect(parsed.index()).toBe(triggerEventOutputBox.index);
  });
});

describe('toCollateralBoxData', () => {
  /**
   * @target should extract WID from R4 and RSN amount from R5
   * @dependencies
   * - ergo-lib-wasm-nodejs
   * @scenario
   * - Build a collateral box with R4 Coll[Byte] and R5 Long
   * - Convert to OutputBox and call toCollateralBoxData
   * @expected
   * - WID and RSN amount are extracted correctly
   */
  it('should extract WID from R4 and RSN amount from R5', () => {
    const data = toCollateralBoxData(collateralOutputBox, sampleRsnTokenId);
    expect(data.wid).toBe(sampleWids[0]);
    expect(data.rsnAmount).toBe(9000n);
  });
});