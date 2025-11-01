import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { selectErgoBoxes } from '@rosen-bridge/ergo-box-selection';
import JsonBigInt from '@rosen-bridge/json-bigint';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { toErgoBoxProxyIterator } from './utils';

/**
 * Represents a fraud box containing RWT tokens to be slashed
 */
export interface FraudBoxData {
  box: ergoLib.ErgoBox;
  wid: string; // Watcher ID hex string (from R4 register)
  rwtAmount: bigint; // RWT tokens in the fraud box
}

/**
 * Represents a collateral box for a specific watcher
 */
export interface CollateralBoxData {
  box: ergoLib.ErgoBox;
  wid: string; // Watcher ID hex string (from R4 register)
  rsnAmount: bigint; // RSN amount in R5 register
}

/**
 * Represents the RWT repository box tracking total RWT and RSN
 */
export interface RWTRepoData {
  box: ergoLib.ErgoBox;
  repoNFT: string; // RepoNFT token ID
  rwtTokenId: string; // RWT token ID
  rsnTokenId: string; // RSN token ID
}

/**
 * SlashTx class handles slashing transactions for fraudulent watchers
 * This is the TypeScript equivalent of the Scala slashFraud transaction
 */
export class SlashTx {
  private static _instance?: SlashTx;

  private constructor(
    private repoAddress: string,
    private collateralAddress: string,
    private cleanupAddress: string,
    private minBoxValue: bigint,
    private txFee: string,
    private logger?: AbstractLogger,
  ) {}

  /**
   * Initializes the singleton instance of SlashTx
   */
  static init = (
    repoAddress: string,
    collateralAddress: string,
    cleanupAddress: string,
    minBoxValue: bigint,
    txFee: string,
    logger?: AbstractLogger,
  ): void => {
    if (SlashTx._instance !== undefined) {
      return;
    }
    SlashTx._instance = new SlashTx(
      repoAddress,
      collateralAddress,
      cleanupAddress,
      minBoxValue,
      txFee,
      logger,
    );
  };

  /**
   * Returns the singleton instance of SlashTx
   */
  static getInstance = (): SlashTx => {
    if (!this._instance) {
      throw new Error('SlashTx instance is not initialized yet');
    }
    return this._instance;
  };

  /**
   * Creates a new SlashTxBuilder instance
   */
  newBuilder = (
    fraudBoxData: FraudBoxData,
    collateralBoxData: CollateralBoxData,
    repoData: RWTRepoData,
    cleanupBox: ergoLib.ErgoBox,
    height: number,
    feeBoxes: ergoLib.ErgoBox[],
    changeAddress?: string,
  ): SlashTxBuilder => {
    return new SlashTxBuilder(
      this.repoAddress,
      this.collateralAddress,
      changeAddress || this.cleanupAddress,
      this.minBoxValue,
      this.txFee,
      fraudBoxData,
      collateralBoxData,
      repoData,
      cleanupBox,
      height,
      feeBoxes,
      this.logger,
    );
  };
}

/**
 * Builder class for creating slash transactions
 * Equivalent to the reference slashRSN implementation
 */
export class SlashTxBuilder {
  constructor(
    private repoAddress: string,
    private collateralAddress: string,
    private changeAddress: string,
    private minBoxValue: bigint,
    private txFee: string,
    private fraudBoxData: FraudBoxData,
    private collateralBoxData: CollateralBoxData,
    private repoData: RWTRepoData,
    private cleanupBox: ergoLib.ErgoBox,
    private height: number,
    private feeBoxes: ergoLib.ErgoBox[],
    private logger?: AbstractLogger,
  ) {
    if (height < 1) {
      throw new Error('Creation height must be a positive integer');
    }
  }

  /**
   * Validates that the slash is possible
   */
  private validateSlash = (): void => {
    const slashedRwtCount = this.fraudBoxData.rwtAmount;
    const collateralRsnAmount = this.collateralBoxData.rsnAmount;

    if (collateralRsnAmount < slashedRwtCount) {
      throw new Error(
        `Impossible case: fraud RWT (${slashedRwtCount}) is more than collateral RSN (${collateralRsnAmount}) for watcher ${this.fraudBoxData.wid}`,
      );
    }

    if (this.fraudBoxData.wid !== this.collateralBoxData.wid) {
      throw new Error(
        `WID mismatch: fraud box WID (${this.fraudBoxData.wid}) != collateral box WID (${this.collateralBoxData.wid})`,
      );
    }
  };

  /**
   * Creates the new repo box with updated RWT and RSN counts
   */
  private createRepoBox = (): ergoLib.ErgoBoxCandidate => {
    const slashedRwtCount = this.fraudBoxData.rwtAmount;

    // Get current token amounts from repo box
    const currentRWT = this.getTokenAmount(
      this.repoData.box,
      this.repoData.rwtTokenId,
    );

    const boxBuilder = new ergoLib.ErgoBoxCandidateBuilder(
      ergoLib.BoxValue.from_i64(
        ergoLib.I64.from_str(this.repoData.box.value().as_i64().to_str()),
      ),
      ergoLib.Contract.pay_to_address(
        ergoLib.Address.from_base58(this.repoAddress),
      ),
      this.height,
    );

    // Add tokens: RepoNFT, RWT (increased), RSN (decreased), and any other tokens
    for (let i = 0; i < this.repoData.box.tokens().len(); i++) {
      const token = this.repoData.box.tokens().get(i);
      const tokenId = token.id().to_str();

      if (tokenId === this.repoData.rwtTokenId) {
        // Increase RWT by slashed amount
        boxBuilder.add_token(
          token.id(),
          ergoLib.TokenAmount.from_i64(
            ergoLib.I64.from_str((currentRWT + slashedRwtCount).toString()),
          ),
        );
      } else {
        // Keep other tokens unchanged (RSN stays the same)
        boxBuilder.add_token(token.id(), token.amount());
      }
    }

    // Set registers: R4 (WIDs) and R5 (RWTs) - keep unchanged
    boxBuilder.set_register_value(4, this.repoData.box.register_value(4)!);
    boxBuilder.set_register_value(5, this.repoData.box.register_value(5)!);

    return boxBuilder.build();
  };

  /**
   * Creates the updated collateral box with reduced RSN token and R5
   */
  private createCollateralBox = (): ergoLib.ErgoBoxCandidate => {
    const slashedRwtCount = this.fraudBoxData.rwtAmount;
    const newRsnAmount = this.collateralBoxData.rsnAmount - slashedRwtCount;

    const boxBuilder = new ergoLib.ErgoBoxCandidateBuilder(
      ergoLib.BoxValue.from_i64(
        ergoLib.I64.from_str(
          this.collateralBoxData.box.value().as_i64().to_str(),
        ),
      ),
      ergoLib.Contract.pay_to_address(
        ergoLib.Address.from_base58(this.collateralAddress),
      ),
      this.height,
    );

    // Add all tokens from original collateral box, reducing RSN by slashed amount
    for (let i = 0; i < this.collateralBoxData.box.tokens().len(); i++) {
      const token = this.collateralBoxData.box.tokens().get(i);
      const tokenId = token.id().to_str();

      if (tokenId === this.repoData.rsnTokenId) {
        // Reduce RSN token by slashed amount
        const currentRsn = BigInt(token.amount().as_i64().to_str());
        const newRsn = currentRsn - slashedRwtCount;
        // Only add token if amount is greater than 0
        if (newRsn > 0n) {
          boxBuilder.add_token(
            token.id(),
            ergoLib.TokenAmount.from_i64(
              ergoLib.I64.from_str(newRsn.toString()),
            ),
          );
        }
      } else {
        boxBuilder.add_token(token.id(), token.amount());
      }
    }

    // Set R4 register (WID) - unchanged
    boxBuilder.set_register_value(
      4,
      this.collateralBoxData.box.register_value(4)!,
    );

    // Set R5 register (RSN amount) - reduced
    boxBuilder.set_register_value(
      5,
      ergoLib.Constant.from_i64(ergoLib.I64.from_str(newRsnAmount.toString())),
    );

    return boxBuilder.build();
  };

  /**
   * Creates the new cleanup box with same ERG value and only cleanup token
   */
  private createCleanupBox = (): ergoLib.ErgoBoxCandidate => {
    const boxBuilder = new ergoLib.ErgoBoxCandidateBuilder(
      ergoLib.BoxValue.from_i64(
        ergoLib.I64.from_str(this.cleanupBox.value().as_i64().to_str()),
      ),
      ergoLib.Contract.new(this.cleanupBox.ergo_tree()),
      this.height,
    );

    // Add cleanup token only
    boxBuilder.add_token(
      this.cleanupBox.tokens().get(0).id(),
      this.cleanupBox.tokens().get(0).amount(),
    );

    return boxBuilder.build();
  };

  /**
   * Creates change box with all remaining ERG, tokens from fee boxes, and slashed RSN
   */
  private createChangeBox = (
    selectedFeeBoxes: ergoLib.ErgoBox[],
  ): ergoLib.ErgoBoxCandidate => {
    // Calculate total input ERG
    const repoValue = BigInt(this.repoData.box.value().as_i64().to_str());
    const collateralValue = BigInt(
      this.collateralBoxData.box.value().as_i64().to_str(),
    );
    const fraudValue = BigInt(this.fraudBoxData.box.value().as_i64().to_str());
    const cleanupValue = BigInt(this.cleanupBox.value().as_i64().to_str());
    let totalInputErg = repoValue + collateralValue + fraudValue + cleanupValue;

    // Add fee box values
    for (const feeBox of selectedFeeBoxes) {
      totalInputErg += BigInt(feeBox.value().as_i64().to_str());
    }

    // Calculate change box value: total inputs - repo output - collateral output - cleanup output - tx fee
    const changeBoxValue =
      totalInputErg -
      repoValue -
      collateralValue -
      cleanupValue -
      BigInt(this.txFee);

    const boxBuilder = new ergoLib.ErgoBoxCandidateBuilder(
      ergoLib.BoxValue.from_i64(
        ergoLib.I64.from_str(changeBoxValue.toString()),
      ),
      ergoLib.Contract.new(this.cleanupBox.ergo_tree()),
      this.height,
    );

    // Aggregate all tokens from fee boxes
    const tokenMap = new Map<string, bigint>();
    for (const feeBox of selectedFeeBoxes) {
      const tokens = feeBox.tokens();
      for (let i = 0; i < tokens.len(); i++) {
        const token = tokens.get(i);
        const tokenId = token.id().to_str();
        const amount = BigInt(token.amount().as_i64().to_str());
        tokenMap.set(tokenId, (tokenMap.get(tokenId) || 0n) + amount);
      }
    }

    // Add slashed RSN amount to change box
    const slashedRwtCount = this.fraudBoxData.rwtAmount;
    tokenMap.set(
      this.repoData.rsnTokenId,
      (tokenMap.get(this.repoData.rsnTokenId) || 0n) + slashedRwtCount,
    );

    // Add all aggregated tokens to change box
    for (const [tokenId, amount] of tokenMap) {
      boxBuilder.add_token(
        ergoLib.TokenId.from_str(tokenId),
        ergoLib.TokenAmount.from_i64(ergoLib.I64.from_str(amount.toString())),
      );
    }

    return boxBuilder.build();
  };

  /**
   * Helper to get token amount from a box
   */
  private getTokenAmount = (box: ergoLib.ErgoBox, tokenId: string): bigint => {
    for (let i = 0; i < box.tokens().len(); i++) {
      const token = box.tokens().get(i);
      if (token.id().to_str() === tokenId) {
        return BigInt(token.amount().as_i64().to_str());
      }
    }
    throw new Error(
      `Token ${tokenId} not found in box ${box.box_id().to_str()}`,
    );
  };

  /**
   * Selects fee boxes to cover the required value
   */
  private selectFeeBoxes = async (
    requiredValue: bigint,
  ): Promise<ergoLib.ErgoBox[]> => {
    if (requiredValue <= 0) {
      return [];
    }

    const feeBoxIterator = toErgoBoxProxyIterator(
      this.feeBoxes[Symbol.iterator](),
    );

    const { covered, boxes: payProxyBoxes } = await selectErgoBoxes(
      { nativeToken: requiredValue, tokens: [] },
      [],
      new Map(),
      feeBoxIterator,
      this.logger,
    );

    if (!covered) {
      throw new Error(
        `Available fee boxes are not enough to cover required value=${requiredValue}`,
      );
    }

    const selectedFeeBoxes = payProxyBoxes.map((proxyBox) =>
      ergoLib.ErgoBox.from_json(JsonBigInt.stringify(proxyBox)),
    );

    this.logger?.debug(
      `Selected ${selectedFeeBoxes.length} fee boxes: ${selectedFeeBoxes.map((box) => box.box_id().to_str()).join(', ')}`,
    );

    return selectedFeeBoxes;
  };

  /**
   * Builds the unsigned slash transaction
   * This spends: repo box, collateral box, fraud box, cleanup box, and optional fee boxes
   * This creates: new repo box (RWT+, RSN-), new collateral box (R5-), new cleanup box, change box
   */
  build = async (): Promise<{
    unsignedTx: ergoLib.UnsignedTransaction;
    inputBoxes: ergoLib.ErgoBox[];
  }> => {
    // Validate slash
    this.validateSlash();

    // Calculate how much ERG we need (outputs need: repo same value, collateral same value, cleanup minBox)
    const repoValue = BigInt(this.repoData.box.value().as_i64().to_str());
    const collateralValue = BigInt(
      this.collateralBoxData.box.value().as_i64().to_str(),
    );
    const fraudValue = BigInt(this.fraudBoxData.box.value().as_i64().to_str());
    const cleanupValue = BigInt(this.cleanupBox.value().as_i64().to_str());
    const inputValue = repoValue + collateralValue + fraudValue + cleanupValue;

    // Output value: repo (same) + collateral (same) + cleanup (minBox) + change (remaining)
    const outputValue = repoValue + collateralValue + this.minBoxValue;
    const requiredValue = outputValue + BigInt(this.txFee) - inputValue;

    const selectedFeeBoxes = await this.selectFeeBoxes(requiredValue);

    // Create output boxes (order: repo, collateral, cleanup, change)
    const newRepoBox = this.createRepoBox();
    const newCollateralBox = this.createCollateralBox();
    const newCleanupBox = this.createCleanupBox();
    const newChangeBox = this.createChangeBox(selectedFeeBoxes);
    const outputBoxes = [
      newRepoBox,
      newCollateralBox,
      newCleanupBox,
      newChangeBox,
    ];

    // Create input boxes: repo, collateral, fraud, cleanup, fee boxes
    const inputBoxes = [
      this.repoData.box,
      this.collateralBoxData.box,
      this.fraudBoxData.box,
      this.cleanupBox,
      ...selectedFeeBoxes,
    ];

    // Build transaction
    const inputErgoBoxes = ergoLib.ErgoBoxes.empty();
    inputBoxes.forEach((box) => inputErgoBoxes.add(box));

    const ergoBoxCandidates = ergoLib.ErgoBoxCandidates.empty();
    outputBoxes.forEach((box) => ergoBoxCandidates.add(box));

    const txBuilder = ergoLib.TxBuilder.new(
      new ergoLib.BoxSelection(
        inputErgoBoxes,
        new ergoLib.ErgoBoxAssetsDataList(),
      ),
      ergoBoxCandidates,
      this.height,
      ergoLib.BoxValue.from_i64(ergoLib.I64.from_str(this.txFee)),
      ergoLib.Address.from_base58(this.changeAddress),
    );

    const unsignedTx = txBuilder.build();

    this.logger?.info(
      `Unsigned slash transaction built with id=${unsignedTx.id().to_str()}`,
    );
    this.logger?.debug(
      `Built unsigned slash transaction: ${unsignedTx.to_json()}`,
    );

    return { unsignedTx, inputBoxes };
  };
}
