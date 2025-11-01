import JsonBigInt from '@rosen-bridge/json-bigint';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { beforeEach, describe, expect, it } from 'vitest';
import { SlashTx, FraudBoxData, CollateralBoxData, RWTRepoData } from '../lib';
import {
  mockRepoWids,
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

  describe('getInstance', () => {
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
  });

  describe('init and getInstance', () => {
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
      const repoAddress = '9iHyKxXs2ZNLMp9N9gbUT9V8gTbsV7HED1C1VhttMfBUMPDyF7r';
      const collateralAddress =
        '9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v';
      const cleanupAddress =
        '9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v';
      const minBoxValue = 1000000n;
      const txFee = '1100000';

      SlashTx.init(
        repoAddress,
        collateralAddress,
        cleanupAddress,
        minBoxValue,
        txFee,
      );

      const instance = SlashTx.getInstance();
      expect(instance).toBeDefined();
      expect(instance['repoAddress']).toEqual(repoAddress);
      expect(instance['collateralAddress']).toEqual(collateralAddress);
      expect(instance['cleanupAddress']).toEqual(cleanupAddress);
      expect(instance['minBoxValue']).toEqual(minBoxValue);
      expect(instance['txFee']).toEqual(txFee);
    });
  });
});

describe('SlashTxBuilder', () => {
  const repoAddress = '9iHyKxXs2ZNLMp9N9gbUT9V8gTbsV7HED1C1VhttMfBUMPDyF7r';
  const collateralAddress =
    '9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v';
  const cleanupAddress = '9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v';
  const minBoxValue = 1000000n;
  const txFee = '1100000';

  beforeEach(() => {
    SlashTx['_instance'] = undefined;
    SlashTx.init(
      repoAddress,
      collateralAddress,
      cleanupAddress,
      minBoxValue,
      txFee,
    );
  });

  describe('constructor', () => {
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
      const fraudBoxData: FraudBoxData = {
        box: {} as ergoLib.ErgoBox,
        wid: mockRepoWids[0],
        rwtAmount: 1000000n,
      };

      const collateralBoxData: CollateralBoxData = {
        box: {} as ergoLib.ErgoBox,
        wid: mockRepoWids[0],
        rsnAmount: 800000n,
      };

      const repoData: RWTRepoData = {
        box: {} as ergoLib.ErgoBox,
        repoNFT: testSlashConfig.repoNFT,
        rwtTokenId: testSlashConfig.rwtTokenId,
        rsnTokenId: testSlashConfig.rsnTokenId,
      };

      expect(() =>
        SlashTx.getInstance().newBuilder(
          fraudBoxData,
          collateralBoxData,
          repoData,
          {} as ergoLib.ErgoBox,
          0,
          [],
        ),
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
      const fraudBoxData: FraudBoxData = {
        box: {} as ergoLib.ErgoBox,
        wid: mockRepoWids[0],
        rwtAmount: 1000000n,
      };

      const collateralBoxData: CollateralBoxData = {
        box: {} as ergoLib.ErgoBox,
        wid: mockRepoWids[0],
        rsnAmount: 800000n,
      };

      const repoData: RWTRepoData = {
        box: {} as ergoLib.ErgoBox,
        repoNFT: testSlashConfig.repoNFT,
        rwtTokenId: testSlashConfig.rwtTokenId,
        rsnTokenId: testSlashConfig.rsnTokenId,
      };

      const slashTxBuilder = SlashTx.getInstance().newBuilder(
        fraudBoxData,
        collateralBoxData,
        repoData,
        {} as ergoLib.ErgoBox,
        1000,
        [],
      );

      expect(slashTxBuilder).toBeDefined();
      expect(slashTxBuilder['height']).toBe(1000);
    });
  });
});

// =============================================================================
// Integration Tests - Full Slash Transaction Generation
// These tests require valid box data. Replace placeholders in testData.ts
// =============================================================================

describe('SlashTx Integration Tests', () => {
  beforeEach(() => {
    SlashTx['_instance'] = undefined;
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
    SlashTx.init(
      testSlashConfig.repoAddress,
      testSlashConfig.collateralAddress,
      testSlashConfig.cleanupAddress,
      testSlashConfig.minBoxValue,
      testSlashConfig.txFee,
    );

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

    // Prepare fraud box data
    const fraudBoxData: FraudBoxData = {
      box: fraudBox,
      wid: mockRepoWids[0],
      rwtAmount: BigInt(fraudBoxJson.assets[0].amount),
    };

    // Prepare collateral box data
    const collateralBoxData: CollateralBoxData = {
      box: collateralBox,
      wid: mockRepoWids[0],
      rsnAmount: 800000n,
    };

    // Prepare repo data
    const repoData: RWTRepoData = {
      box: repoBox,
      repoNFT: testSlashConfig.repoNFT,
      rwtTokenId: testSlashConfig.rwtTokenId,
      rsnTokenId: testSlashConfig.rsnTokenId,
    };

    // Build transaction
    const slashTxBuilder = SlashTx.getInstance().newBuilder(
      fraudBoxData,
      collateralBoxData,
      repoData,
      cleanupBox,
      testSlashConfig.repoWids.length, // height from the fraudBoxJson
      feeBoxes,
    );
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
    const slashedRwtAmount = fraudBoxData.rwtAmount;
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
    expect(newRepoRsn).toBe(originalRepoRsn); // RSN stays the same in repo

    // Output 1: Collateral box with reduced RSN in R5
    const collateralOutput = outputs.get(1);
    expect(collateralOutput.value().as_i64().to_str()).toBe(
      collateralBox.value().as_i64().to_str(),
    );
    const r5Register = collateralOutput.register_value(5);
    expect(r5Register).toBeDefined();

    const newCollateralRsn = BigInt(r5Register!.to_i64().to_str());
    expect(newCollateralRsn).toBe(
      collateralBoxData.rsnAmount - slashedRwtAmount,
    );

    // Output 2: Cleanup box
    const cleanupOutput = outputs.get(2);
    expect(cleanupOutput.value().as_i64().to_str()).toBe(
      testSlashConfig.minBoxValue.toString(),
    );
    expect(cleanupOutput.tokens().len()).toBe(1);
    expect(cleanupOutput.tokens().get(0).id().to_str()).toBe(
      cleanupBox.tokens().get(0).id().to_str(),
    );

    // Output 3: Change box (if exists)
    if (outputs.len() > 3) {
      const changeOutput = outputs.get(3);
      expect(changeOutput.value().as_i64().as_num()).toBeGreaterThan(0);
    }
  });

  /**
   * @target should fail when insufficient ERG in inputs
   */
  it('should fail when insufficient ERG in inputs', async () => {
    SlashTx.init(
      testSlashConfig.repoAddress,
      testSlashConfig.collateralAddress,
      testSlashConfig.cleanupAddress,
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

    const fraudBoxData: FraudBoxData = {
      box: fraudBox,
      wid: mockRepoWids[0],
      rwtAmount: BigInt(fraudBoxJson.assets[0].amount),
    };

    const rsnAmount = BigInt(
      '0x' + collateralBoxJson.additionalRegisters.R5.slice(2),
    );

    const collateralBoxData: CollateralBoxData = {
      box: collateralBox,
      wid: mockRepoWids[0],
      rsnAmount: rsnAmount,
    };

    const repoData: RWTRepoData = {
      box: repoBox,
      repoNFT: testSlashConfig.repoNFT,
      rwtTokenId: testSlashConfig.rwtTokenId,
      rsnTokenId: testSlashConfig.rsnTokenId,
    };

    const slashTxBuilder = SlashTx.getInstance().newBuilder(
      fraudBoxData,
      collateralBoxData,
      repoData,
      cleanupBox,
      testSlashConfig.repoWids.length,
      [], // No fee boxes
    );

    await expect(slashTxBuilder.build()).rejects.toThrow();
  });

  /**
   * @target should fail when fraud box has zero RWT
   */
  it('should fail when fraud box has zero RWT', async () => {
    SlashTx.init(
      testSlashConfig.repoAddress,
      testSlashConfig.collateralAddress,
      testSlashConfig.cleanupAddress,
      testSlashConfig.minBoxValue,
      testSlashConfig.txFee,
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
    const feeBoxes = feeBoxesJson.map((json) =>
      ergoLib.ErgoBox.from_json(JsonBigInt.stringify(json)),
    );

    const fraudBoxData: FraudBoxData = {
      box: fraudBox,
      wid: mockRepoWids[0],
      rwtAmount: 0n, // Zero RWT
    };

    const rsnAmount = BigInt(
      '0x' + collateralBoxJson.additionalRegisters.R5.slice(2),
    );

    const collateralBoxData: CollateralBoxData = {
      box: collateralBox,
      wid: mockRepoWids[0],
      rsnAmount: rsnAmount,
    };

    const repoData: RWTRepoData = {
      box: repoBox,
      repoNFT: testSlashConfig.repoNFT,
      rwtTokenId: testSlashConfig.rwtTokenId,
      rsnTokenId: testSlashConfig.rsnTokenId,
    };

    const slashTxBuilder = SlashTx.getInstance().newBuilder(
      fraudBoxData,
      collateralBoxData,
      repoData,
      cleanupBox,
      testSlashConfig.repoWids.length,
      feeBoxes,
    );

    await expect(slashTxBuilder.build()).rejects.toThrow();
  });

  /**
   * @target should fail when collateral RSN is less than slashed amount
   */
  it('should fail when collateral RSN is less than slashed amount', async () => {
    SlashTx.init(
      testSlashConfig.repoAddress,
      testSlashConfig.collateralAddress,
      testSlashConfig.cleanupAddress,
      testSlashConfig.minBoxValue,
      testSlashConfig.txFee,
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
    const feeBoxes = feeBoxesJson.map((json) =>
      ergoLib.ErgoBox.from_json(JsonBigInt.stringify(json)),
    );

    const fraudBoxData: FraudBoxData = {
      box: fraudBox,
      wid: mockRepoWids[0],
      rwtAmount: BigInt(fraudBoxJson.assets[0].amount),
    };

    const collateralBoxData: CollateralBoxData = {
      box: collateralBox,
      wid: mockRepoWids[0],
      rsnAmount: 10n, // RSN less than slashed RWT amount
    };

    const repoData: RWTRepoData = {
      box: repoBox,
      repoNFT: testSlashConfig.repoNFT,
      rwtTokenId: testSlashConfig.rwtTokenId,
      rsnTokenId: testSlashConfig.rsnTokenId,
    };

    const slashTxBuilder = SlashTx.getInstance().newBuilder(
      fraudBoxData,
      collateralBoxData,
      repoData,
      cleanupBox,
      testSlashConfig.repoWids.length,
      feeBoxes,
    );

    await expect(slashTxBuilder.build()).rejects.toThrow();
  });
});
