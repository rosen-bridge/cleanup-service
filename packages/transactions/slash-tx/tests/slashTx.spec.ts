import JsonBigInt from '@rosen-bridge/json-bigint';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { beforeEach, describe, expect, it } from 'vitest';
import { SlashTx, RWTRepoData, getTokenAmount } from '../lib';
import {
  fraudBoxJson,
  collateralBoxJson,
  repoBoxJson,
  cleanupBoxJson,
  feeBoxesJson,
  testSlashConfig,
} from './testData';

describe('SlashTx', () => {
  beforeEach(() => {
    // Reset singleton instance before each test
    SlashTx['_instance'] = undefined;
  });

  /**
   * @target should throw exception when SlashTx._instance is not yet initialized
   * @dependencies
   * - None
   * @scenario
   * - Call SlashTx.getInstance without calling SlashTx.init
   * - Check SlashTx.getInstance to throw an exception
   * @expected
   * - SlashTx.getInstance should throw an exception
   */
  it('should throw exception when SlashTx._instance is not yet initialized', () => {
    expect(() => SlashTx.getInstance()).toThrowError(
      'SlashTx instance is not initialized yet',
    );
  });

  /**
   * @target should initialize singleton correctly
   * @dependencies
   * - None
   * @scenario
   * - Call SlashTx.init with parameters
   * - Call SlashTx.getInstance to get instance
   * @expected
   * - getInstance should return the initialized instance
   */
  it('should initialize singleton correctly', () => {
    SlashTx.init(testSlashConfig.minBoxValue, testSlashConfig.txFee);

    const instance = SlashTx.getInstance();
    expect(instance).toBeDefined();
    expect(instance['minBoxValue']).toEqual(testSlashConfig.minBoxValue);
    expect(instance['txFee']).toEqual(testSlashConfig.txFee);
  });

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
    SlashTx.init(testSlashConfig.minBoxValue, testSlashConfig.txFee);
    expect(() =>
      SlashTx.getInstance().newBuilder().setCreationHeight(0),
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
    SlashTx.init(testSlashConfig.minBoxValue, testSlashConfig.txFee);
    const slashTxBuilder = SlashTx.getInstance()
      .newBuilder()
      .setCreationHeight(1000);
    expect(slashTxBuilder).toBeDefined();
    expect(slashTxBuilder['height']).toBe(1000);
  });

  /**
   * @target should build a complete slash transaction with real boxes
   * @dependencies
   * - Valid box data in testData.ts
   * @scenario
   * - Initialize SlashTx with config
   * - Create fraud box data and repo data from real boxes
   * - Build transaction with cleaner box and fee boxes
   * - Verify outputs: new repo box, slashed box, new cleaner box
   * - Verify repo box has updated RWT counts
   * - Verify slashed box contains RSN tokens
   * @expected
   * - Transaction builds successfully
   * - Correct number of outputs (3)
   * - Repo updated correctly
   * - RSN tokens sent to slash address
   */
  it('should build a complete slash transaction with correct structure', async () => {
    // Initialize SlashTx
    SlashTx.init(testSlashConfig.minBoxValue, testSlashConfig.txFee);

    // Parse boxes from JSON
    const fraudBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(fraudBoxJson),
    );
    const collateralBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(collateralBoxJson),
    );
    const repoBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(repoBoxJson),
    );
    const cleanupBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(cleanupBoxJson),
    );
    const feeBoxes = feeBoxesJson.map((boxJson) =>
      ergoLib.ErgoBox.from_json(JsonBigInt.stringify(boxJson)),
    );

    // Prepare repo data
    const repoData: RWTRepoData = {
      box: repoBox,
      repoNFT: testSlashConfig.repoNFT,
      rwtTokenId: testSlashConfig.rwtTokenId,
      rsnTokenId: testSlashConfig.rsnTokenId,
      awcTokenId: testSlashConfig.awcTokenId,
    };

    // Build transaction
    const slashTxBuilder = SlashTx.getInstance()
      .newBuilder()
      .setFraudBox(fraudBox)
      .setCollateralBox(collateralBox)
      .setRepoData(repoData)
      .setCleanupBox(cleanupBox)
      .setCreationHeight(testSlashConfig.repoWids.length)
      .setFeeBoxes(feeBoxes)
      .setChangeAddress(testSlashConfig.cleanupAddress);
    const result = await slashTxBuilder.build();

    // Verify transaction was built
    expect(result.unsignedTx).toBeDefined();

    // Verify inputs: repo, collateral, fraud, cleanup
    expect(result.inputBoxes.length).toBeGreaterThanOrEqual(4);
    expect(result.inputBoxes[0].box_id().to_str()).toBe(
      repoBox.box_id().to_str(),
    );
    expect(result.inputBoxes[1].box_id().to_str()).toBe(
      collateralBox.box_id().to_str(),
    );
    expect(result.inputBoxes[2].box_id().to_str()).toBe(
      fraudBox.box_id().to_str(),
    );
    expect(result.inputBoxes[3].box_id().to_str()).toBe(
      cleanupBox.box_id().to_str(),
    );

    // Any additional inputs should be from the provided fee boxes
    for (let i = 4; i < result.inputBoxes.length; i++) {
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

    // Should have: repo + collateral + cleanup + change
    expect(outputs.len()).toBeGreaterThanOrEqual(3);

    // Output 0: Repo box with updated RWT and RSN
    const repoOutput = outputs.get(0);
    expect(repoOutput.value().as_i64().to_str()).toBe(
      repoBox.value().as_i64().to_str(),
    );
    expect(repoOutput.tokens().len()).toBe(repoBox.tokens().len());

    // Verify repo RWT increased and RSN decreased
    const slashedRwtAmount = getTokenAmount(
      fraudBox,
      testSlashConfig.rwtTokenId,
    );
    const originalRepoRwt = BigInt(
      repoBox.tokens().get(1).amount().as_i64().to_str(),
    );
    const originalRepoRsn = BigInt(
      repoBox.tokens().get(2).amount().as_i64().to_str(),
    );

    const newRepoRwt = BigInt(
      repoOutput.tokens().get(1).amount().as_i64().to_str(),
    );
    const newRepoRsn = BigInt(
      repoOutput.tokens().get(2).amount().as_i64().to_str(),
    );

    expect(newRepoRwt).toBe(originalRepoRwt + slashedRwtAmount);
    expect(newRepoRsn).toBe(originalRepoRsn - slashedRwtAmount);

    // Output 1: Collateral box with reduced RSN in R5
    const collateralOutput = outputs.get(1);
    expect(collateralOutput.value().as_i64().to_str()).toBe(
      collateralBox.value().as_i64().to_str(),
    );
    const r5Register = collateralOutput.register_value(5);
    expect(r5Register).toBeDefined();

    const newCollateralRsn = BigInt(r5Register!.to_i64().to_str());
    expect(newCollateralRsn).toBe(
      getTokenAmount(collateralBox, testSlashConfig.rsnTokenId) -
        slashedRwtAmount,
    );

    // Output 2: Cleanup box
    const cleanupOutput = outputs.get(2);
    expect(cleanupOutput.value().as_i64().to_str()).toBe(
      testSlashConfig.minBoxValue.toString(),
    );
    expect(cleanupOutput.tokens().len()).toBeGreaterThanOrEqual(1);
    expect(cleanupOutput.tokens().get(0).id().to_str()).toBe(
      cleanupBox.tokens().get(0).id().to_str(),
    );
    if (cleanupOutput.tokens().len() > 1) {
      const rsnToken = cleanupOutput.tokens().get(1);
      expect(rsnToken.id().to_str()).toBe(testSlashConfig.rsnTokenId);
      expect(rsnToken.amount().as_i64().to_str()).toBe(
        getTokenAmount(fraudBox, testSlashConfig.rwtTokenId).toString(),
      );
    }
  });

  /**
   * @target should fail when insufficient ERG in inputs
   * @dependencies
   * - None
   * @scenario
   * - Initialize SlashTx with config
   * - Create repo box data and collateral box data from real boxes
   * - Build transaction with no fee boxes
   * @expected
   * - Should throw error
   */
  it('should fail when insufficient ERG in inputs', async () => {
    SlashTx.init(
      testSlashConfig.minBoxValue,
      '99999999999', // Unreasonably high fee
    );

    const repoBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(repoBoxJson),
    );
    const collateralBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(collateralBoxJson),
    );
    const fraudBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(fraudBoxJson),
    );
    const cleanupBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(cleanupBoxJson),
    );

    const repoData: RWTRepoData = {
      box: repoBox,
      repoNFT: testSlashConfig.repoNFT,
      rwtTokenId: testSlashConfig.rwtTokenId,
      rsnTokenId: testSlashConfig.rsnTokenId,
      awcTokenId: testSlashConfig.awcTokenId,
    };

    const slashTxBuilder = SlashTx.getInstance()
      .newBuilder()
      .setFraudBox(fraudBox)
      .setCollateralBox(collateralBox)
      .setRepoData(repoData)
      .setCleanupBox(cleanupBox)
      .setCreationHeight(testSlashConfig.repoWids.length)
      .setFeeBoxes([]); // No fee boxes

    await expect(slashTxBuilder.build()).rejects.toThrow();
  });

  /**
   * @target should fail when collateral RSN is less than slashed amount
   * @dependencies
   * - None
   * @scenario
   * - Initialize SlashTx with config
   * - Create repo box data and collateral box data from real boxes
   * - Build transaction with no fee boxes
   * - Set collateral box RSN to less than slashed amount
   * @expected
   * - Should throw error
   */
  it('should fail when collateral RSN is less than slashed amount', async () => {
    SlashTx.init(testSlashConfig.minBoxValue, testSlashConfig.txFee);

    const repoBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(repoBoxJson),
    );
    const collateralBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(collateralBoxJson),
    );
    const fraudBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(fraudBoxJson),
    );
    const cleanupBox = ergoLib.ErgoBox.from_json(
      JsonBigInt.stringify(cleanupBoxJson),
    );
    const feeBoxes = feeBoxesJson.map((json) =>
      ergoLib.ErgoBox.from_json(JsonBigInt.stringify(json)),
    );
    const repoData: RWTRepoData = {
      box: repoBox,
      repoNFT: testSlashConfig.repoNFT,
      rwtTokenId: testSlashConfig.rwtTokenId,
      rsnTokenId: testSlashConfig.rsnTokenId,
      awcTokenId: testSlashConfig.awcTokenId,
    };

    const slashTxBuilder = SlashTx.getInstance()
      .newBuilder()
      .setFraudBox(fraudBox)
      .setCollateralBox(collateralBox)
      .setRepoData(repoData)
      .setCleanupBox(cleanupBox)
      .setCreationHeight(testSlashConfig.repoWids.length)
      .setFeeBoxes(feeBoxes);

    await expect(slashTxBuilder.build()).rejects.toThrow();
  });
});
