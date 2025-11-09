import JsonBigInt from '@rosen-bridge/json-bigint';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { beforeEach, describe, expect, it } from 'vitest';
import { FraudTx, TriggerEventData } from '../lib';
import {
  mockWids,
  triggerEventBoxJson,
  cleanerBoxJson,
  feeBoxesJson,
  testFraudConfig,
} from './testData';

describe('FraudTx', () => {
  beforeEach(() => {
    // Reset singleton instance before each test
    FraudTx['_instance'] = undefined;
  });

  describe('getInstance', () => {
    /**
     * @target should throw exception when FraudTx._instance is not yet initialized
     * @dependencies
     * - None
     * @scenario
     * - Call FraudTx.getInstance without calling FraudTx.init
     * - Check FraudTx.getInstance to throw an exception
     * @expected
     * - FraudTx.getInstance should throw an exception
     */
    it('should throw exception when FraudTx._instance is not yet initialized', () => {
      expect(() => FraudTx.getInstance()).toThrowError(
        'FraudTx instance is not initialized yet',
      );
    });
  });

  describe('init and getInstance', () => {
    /**
     * @target should initialize singleton correctly
     * @dependencies
     * - None
     * @scenario
     * - Call FraudTx.init with parameters
     * - Call FraudTx.getInstance to get instance
     * @expected
     * - getInstance should return the initialized instance
     */
    it('should initialize singleton correctly', () => {
      const fraudAddress =
        '9iHyKxXs2ZNLMp9N9gbUT9V8gTbsV7HED1C1VhttMfBUMPDyF7r';
      const cleanerAddress =
        '9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v';
      const rwtTokenId =
        'a908bf2e9d0452f4c9def5b0f0d6e8f0e3c8d5a1e4f8b2c9d0e3f8a5b1c4d7e0';
      const minBoxValue = 1000000n;
      const txFee = '1100000';

      FraudTx.init(
        fraudAddress,
        cleanerAddress,
        rwtTokenId,
        minBoxValue,
        txFee,
      );

      const instance = FraudTx.getInstance();
      expect(instance).toBeDefined();
      expect(instance['fraudAddress']).toEqual(fraudAddress);
      expect(instance['cleanerAddress']).toEqual(cleanerAddress);
      expect(instance['rwtTokenId']).toEqual(rwtTokenId);
      expect(instance['minBoxValue']).toEqual(minBoxValue);
      expect(instance['txFee']).toEqual(txFee);
    });
  });
});

describe('FraudTxBuilder', () => {
  const fraudAddress = '9iHyKxXs2ZNLMp9N9gbUT9V8gTbsV7HED1C1VhttMfBUMPDyF7r';
  const cleanerAddress = '9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v';
  const rwtTokenId =
    'a908bf2e9d0452f4c9def5b0f0d6e8f0e3c8d5a1e4f8b2c9d0e3f8a5b1c4d7e0';
  const minBoxValue = 1000000n;
  const txFee = '1100000';

  beforeEach(() => {
    FraudTx['_instance'] = undefined;
    FraudTx.init(fraudAddress, cleanerAddress, rwtTokenId, minBoxValue, txFee);
  });

  describe('setCreationHeight', () => {
    /**
     * @target should throw error when height is invalid
     * @dependencies
     * - None
     * @scenario
     * - Create a builder with height = 0
     * @expected
     * - Should throw error
     */
    it('should throw error when height is invalid', () => {
      expect(() =>
        FraudTx.getInstance().newBuilder().setCreationHeight(0),
      ).toThrow('Creation height must be a positive integer');
    });

    /**
     * @target should create builder successfully when height is valid
     * @dependencies
     * - None
     * @scenario
     * - Create a builder with height = 1000
     * @expected
     * - Should return builder instance
     * - Height should be set
     */
    it('should create builder successfully when height is valid', () => {
      const fraudTxBuilder = FraudTx.getInstance()
        .newBuilder()
        .setCreationHeight(1000);

      expect(fraudTxBuilder).toBeDefined();
      expect(fraudTxBuilder['height']).toBe(1000);
    });

    /**
     * @target should support method chaining
     * @dependencies
     * - None
     * @scenario
     * - Chain multiple setter methods
     * @expected
     * - Should return builder instance for chaining
     */
    it('should support method chaining', () => {
      const triggerEventData: TriggerEventData = {
        box: {} as ergoLib.ErgoBox,
        wids: mockWids,
        rwtAmount: 3000000n,
      };

      const fraudTxBuilder = FraudTx.getInstance()
        .newBuilder()
        .setTriggerEventData(triggerEventData)
        .setCleanerBox({} as ergoLib.ErgoBox)
        .setCreationHeight(1000)
        .setFeeBoxes([]);

      expect(fraudTxBuilder).toBeDefined();
      expect(fraudTxBuilder['height']).toBe(1000);
    });
  });
});

// =============================================================================
// Integration Tests - Full Fraud Transaction Generation
// These tests require valid box data. Replace placeholders in testData.ts
// =============================================================================

describe('FraudTx Integration Tests', () => {
  beforeEach(() => {
    FraudTx['_instance'] = undefined;
  });

  /**
   * @target should build a complete fraud transaction with correct structure
   * @dependencies
   * - Valid box data in testData.ts
   * @scenario
   * - Initialize FraudTx with config
   * - Create trigger event data from real box
   * - Build transaction with cleaner box and many fee boxes
   * - Verify correct inputs used (trigger + cleaner + only needed fee boxes)
   * - Verify correct fraud boxes created (one per WID with equal RWT distribution)
   * - Verify cleaner box preserves all tokens
   * - Verify transaction balances correctly
   * @expected
   * - Transaction builds successfully
   * - Only necessary inputs are used
   * - Fraud boxes have correct properties (value, RWT amount, WID in R4)
   * - Cleaner box preserves cleanup token
   * - Transaction fee matches expected
   */
  it('should build a complete fraud transaction with correct structure', async () => {
    // Initialize FraudTx
    FraudTx.init(
      testFraudConfig.fraudAddress,
      testFraudConfig.cleanerAddress,
      testFraudConfig.rwtTokenId,
      testFraudConfig.minBoxValue,
      testFraudConfig.txFee,
    );

    // Parse boxes from JSON
    const triggerEventBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(triggerEventBoxJson),
    );
    const cleanerBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(cleanerBoxJson),
    );
    const feeBoxes = feeBoxesJson.map((boxJson) =>
      ergoLib.ErgoBox.from_json(JsonBigInt.stringify(boxJson)),
    );

    // Prepare trigger event data
    const triggerEventData: TriggerEventData = {
      box: triggerEventBox,
      wids: mockWids,
      rwtAmount: BigInt(triggerEventBoxJson.assets[0].amount),
    };

    // Build transaction
    // Use fraud address as change address (different from cleaner address)
    const fraudTxBuilder = FraudTx.getInstance()
      .newBuilder()
      .setTriggerEventData(triggerEventData)
      .setCleanerBox(cleanerBox)
      .setCreationHeight(1000)
      .setFeeBoxes(feeBoxes)
      .setChangeAddress(testFraudConfig.fraudAddress);
    const result = await fraudTxBuilder.build();

    // Verify transaction was built
    expect(result.unsignedTx).toBeDefined();

    // Verify inputs: must include trigger box and cleaner box, may include fee boxes
    expect(result.inputBoxes.length).toBeGreaterThanOrEqual(2);
    expect(result.inputBoxes[0].box_id().to_str()).toBe(
      triggerEventBox.box_id().to_str(),
    );
    expect(result.inputBoxes[1].box_id().to_str()).toBe(
      cleanerBox.box_id().to_str(),
    );

    // Any additional inputs should be from the provided fee boxes
    for (let i = 2; i < result.inputBoxes.length; i++) {
      const inputBoxId = result.inputBoxes[i].box_id().to_str();
      const isFeeBox = feeBoxes.some(
        (fb) => fb.box_id().to_str() === inputBoxId,
      );
      expect(isFeeBox, `Input ${i} should be from provided fee boxes`).toBe(
        true,
      );
    }

    // Verify outputs
    const outputs = result.unsignedTx.output_candidates();
    const expectedFraudBoxCount = mockWids.length;

    // Output order: fraud boxes (0..n-1), cleaner box (n), change box (n+1), miner fee (n+2)
    expect(outputs.len()).toBeGreaterThanOrEqual(expectedFraudBoxCount + 2);

    const rwtPerFraud = triggerEventData.rwtAmount / BigInt(mockWids.length);

    // Verify fraud boxes (first n outputs)
    const fraudBoxes: ergoLib.ErgoBoxCandidate[] = [];
    for (let i = 0; i < expectedFraudBoxCount; i++) {
      fraudBoxes.push(outputs.get(i));
    }
    expect(fraudBoxes.length).toBe(expectedFraudBoxCount);

    // Cleaner box is right after fraud boxes
    const cleanerBoxOutput = outputs.get(expectedFraudBoxCount);

    for (let i = 0; i < fraudBoxes.length; i++) {
      const fraudBox = fraudBoxes[i];

      // Each fraud box should have minBoxValue
      expect(fraudBox.value().as_i64().to_str()).toBe(
        testFraudConfig.minBoxValue.toString(),
      );

      // Each fraud box should have RWT token with correct amount
      expect(fraudBox.tokens().len()).toBe(1);
      const token = fraudBox.tokens().get(0);
      expect(token.id().to_str()).toBe(testFraudConfig.rwtTokenId);
      expect(token.amount().as_i64().to_str()).toBe(rwtPerFraud.toString());

      // Each fraud box should have a WID in R4
      const r4 = fraudBox.register_value(4);
      expect(r4).toBeDefined();
      expect(fraudBox.creation_height()).toBe(1000);
    }

    // Verify cleaner box preserves input cleaner box
    expect(cleanerBoxOutput).toBeDefined();
    expect(cleanerBoxOutput.value().as_i64().to_str()).toBe(
      cleanerBox.value().as_i64().to_str(),
    );
    
    const cleanerTokens = cleanerBoxOutput.tokens();
    expect(cleanerTokens.len()).toBe(cleanerBox.tokens().len());
    
    for (let i = 0; i < cleanerTokens.len(); i++) {
      const outputToken = cleanerTokens.get(i);
      const inputToken = cleanerBox.tokens().get(i);
      expect(outputToken.id().to_str()).toBe(inputToken.id().to_str());
      expect(outputToken.amount().as_i64().to_str()).toBe(
        inputToken.amount().as_i64().to_str(),
      );
    }

    // Change box is after cleaner box
    const changeBoxOutput = outputs.get(expectedFraudBoxCount + 1);
    expect(changeBoxOutput).toBeDefined();

    // Miner fee box is last
    const minerFeeBox = outputs.get(expectedFraudBoxCount + 2);
    expect(minerFeeBox).toBeDefined();
    expect(minerFeeBox.value().as_i64().to_str()).toBe(testFraudConfig.txFee);
  });

  /**
   * @target should fail when insufficient ERG in inputs
   */
  it('should fail when insufficient ERG in inputs', async () => {
    FraudTx.init(
      testFraudConfig.fraudAddress,
      testFraudConfig.cleanerAddress,
      testFraudConfig.rwtTokenId,
      testFraudConfig.minBoxValue,
      '999999999999', // Unreasonably high fee
    );

    const triggerEventBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(triggerEventBoxJson),
    );
    const cleanerBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(cleanerBoxJson),
    );

    const triggerEventData: TriggerEventData = {
      box: triggerEventBox,
      wids: mockWids,
      rwtAmount: BigInt(triggerEventBoxJson.assets[0].amount),
    };

    const fraudTxBuilder = FraudTx.getInstance()
      .newBuilder()
      .setTriggerEventData(triggerEventData)
      .setCleanerBox(cleanerBox)
      .setCreationHeight(1000)
      .setFeeBoxes([]) // No fee boxes
      .setChangeAddress(testFraudConfig.fraudAddress);

    await expect(fraudTxBuilder.build()).rejects.toThrow();
  });

  /**
   * @target should fail when RWT amount is zero
   */
  it('should fail when RWT amount is zero', async () => {
    FraudTx.init(
      testFraudConfig.fraudAddress,
      testFraudConfig.cleanerAddress,
      testFraudConfig.rwtTokenId,
      testFraudConfig.minBoxValue,
      testFraudConfig.txFee,
    );

    const triggerEventBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(triggerEventBoxJson),
    );
    const cleanerBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(cleanerBoxJson),
    );

    const triggerEventData: TriggerEventData = {
      box: triggerEventBox,
      wids: mockWids,
      rwtAmount: 0n, // No RWT
    };

    const fraudTxBuilder = FraudTx.getInstance()
      .newBuilder()
      .setTriggerEventData(triggerEventData)
      .setCleanerBox(cleanerBox)
      .setCreationHeight(1000)
      .setFeeBoxes([])
      .setChangeAddress(testFraudConfig.fraudAddress);

    await expect(fraudTxBuilder.build()).rejects.toThrow();
  });

  /**
   * @target should fail with empty WIDs array
   */
  it('should fail with empty WIDs array', async () => {
    FraudTx.init(
      testFraudConfig.fraudAddress,
      testFraudConfig.cleanerAddress,
      testFraudConfig.rwtTokenId,
      testFraudConfig.minBoxValue,
      testFraudConfig.txFee,
    );

    const triggerEventBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(triggerEventBoxJson),
    );
    const cleanerBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(cleanerBoxJson),
    );

    const triggerEventData: TriggerEventData = {
      box: triggerEventBox,
      wids: [], // Empty WIDs
      rwtAmount: BigInt(triggerEventBoxJson.assets[0].amount),
    };

    const fraudTxBuilder = FraudTx.getInstance()
      .newBuilder()
      .setTriggerEventData(triggerEventData)
      .setCleanerBox(cleanerBox)
      .setCreationHeight(1000)
      .setFeeBoxes([])
      .setChangeAddress(testFraudConfig.fraudAddress);

    await expect(fraudTxBuilder.build()).rejects.toThrow();
  });
});
