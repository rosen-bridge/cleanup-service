import { describe, expect, it } from 'vitest';

import { deserializeTxForBoxLookup } from '../src/services/boxLookup/txDeserializer';
import {
  deserializerExpectedFirstInputBoxId,
  deserializerExpectedFirstOutputBoxId,
  deserializerExpectedOutputsLen,
  deserializerExpectedTxId,
  deserializerTxBase64,
} from './testData';
import { makeTxPotEntity } from './testUtils';

describe('deserializeTxForBoxLookup', () => {
  /**
   * @target should project a real txpot serialized ergo transaction to box-lookup DeserializedTx
   * @dependencies
   * - ergo-lib-wasm-nodejs Transaction.sigma_serialize_bytes
   * - ergo-lib-wasm-nodejs Transaction.sigma_parse_bytes
   * @scenario
   * - Deserialize via deserializeTxForBoxLookup
   * @expected
   * - id, inputs[].boxId and outputs[].boxId are correctly projected
   */
  it('should project a real txpot serialized ergo transaction to box-lookup DeserializedTx', () => {
    const projected = deserializeTxForBoxLookup(makeTxPotEntity(deserializerTxBase64));

    expect(projected.id).toBe(deserializerExpectedTxId);
    expect(projected.inputs[0].boxId).toBe(deserializerExpectedFirstInputBoxId);
    expect(projected.outputs).toHaveLength(deserializerExpectedOutputsLen);
    expect(projected.outputs[0].boxId).toBe(deserializerExpectedFirstOutputBoxId);
  });
});