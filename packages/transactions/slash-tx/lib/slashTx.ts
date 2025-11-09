import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { selectErgoBoxes } from '@rosen-bridge/ergo-box-selection';
import JsonBigInt from '@rosen-bridge/json-bigint';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { createChangeBox, toErgoBoxProxyIterator } from './utils';

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
  newBuilder = (): SlashTxBuilder => {
    return new SlashTxBuilder(
      this.repoAddress,
      this.collateralAddress,
      this.cleanupAddress,
      this.minBoxValue,
      this.txFee,
      this.logger,
    );
  };
}

/**
 * Builder class for creating slash transactions
 */
export class SlashTxBuilder {
  private fraudBoxData: FraudBoxData;
  private collateralBoxData: CollateralBoxData;
  private repoData: RWTRepoData;
  private cleanupBox: ergoLib.ErgoBox;
  private height: number;
  private feeBoxes: ergoLib.ErgoBox[];
  private changeAddress: string;

  constructor(
    private repoAddress: string,
    private collateralAddress: string,
    private defaultChangeAddress: string,
    private minBoxValue: bigint,
    private txFee: string,
    private logger?: AbstractLogger,
  ) {
    this.changeAddress = defaultChangeAddress;
  }

  setFraudBoxData = (fraudBoxData: FraudBoxData): SlashTxBuilder => {
    this.fraudBoxData = fraudBoxData;
    this.logger?.debug(`Fraud box set with WID ${fraudBoxData.wid}`);
    return this;
  };

  setCollateralBoxData = (
    collateralBoxData: CollateralBoxData,
  ): SlashTxBuilder => {
    this.collateralBoxData = collateralBoxData;
    this.logger?.debug(`Collateral box set with WID ${collateralBoxData.wid}`);
    return this;
  };

  setRepoData = (repoData: RWTRepoData): SlashTxBuilder => {
    this.repoData = repoData;
    this.logger?.debug(`Repo data set`);
    return this;
  };

  setCleanupBox = (cleanupBox: ergoLib.ErgoBox): SlashTxBuilder => {
    this.cleanupBox = cleanupBox;
    this.logger?.debug(`Cleanup box set with id=${cleanupBox.box_id().to_str()}`);
    return this;
  };

  setCreationHeight = (height: number): SlashTxBuilder => {
    if (height < 1) {
      throw new Error('Creation height must be a positive integer');
    }
    this.height = height;
    this.logger?.debug(`Creation height set to ${height}`);
    return this;
  };

  setFeeBoxes = (feeBoxes: ergoLib.ErgoBox[]): SlashTxBuilder => {
    this.feeBoxes = feeBoxes;
    this.logger?.debug(`Fee boxes set: ${feeBoxes.length} boxes available`);
    return this;
  };

  setChangeAddress = (address: string): SlashTxBuilder => {
    this.changeAddress = address;
    this.logger?.debug(`Change address set to ${address}`);
    return this;
  };

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
   * Creates the new cleanup box preserving input cleanup box
   */
  private createCleanupBox = (): ergoLib.ErgoBoxCandidate => {
    const boxBuilder = new ergoLib.ErgoBoxCandidateBuilder(
      this.cleanupBox.value(),
      ergoLib.Contract.new(this.cleanupBox.ergo_tree()),
      this.height,
    );

    for (let i = 0; i < this.cleanupBox.tokens().len(); i++) {
      const token = this.cleanupBox.tokens().get(i);
      boxBuilder.add_token(token.id(), token.amount());
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

    const { covered, boxes: payProxyBoxes } = await selectErgoBoxes(
      { nativeToken: requiredValue, tokens: [] },
      [],
      new Map(),
      toErgoBoxProxyIterator(this.feeBoxes),
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
   * Spends: repo box, collateral box, fraud box, cleanup box, and optional fee boxes
   * Creates: new repo box, new collateral box, new cleanup box, and change box
   */
  build = async (): Promise<{
    unsignedTx: ergoLib.UnsignedTransaction;
    inputBoxes: ergoLib.ErgoBox[];
  }> => {
    this.validateSlash();

    const repoValue = BigInt(this.repoData.box.value().as_i64().to_str());
    const collateralValue = BigInt(
      this.collateralBoxData.box.value().as_i64().to_str(),
    );
    const fraudValue = BigInt(this.fraudBoxData.box.value().as_i64().to_str());
    const cleanupValue = BigInt(this.cleanupBox.value().as_i64().to_str());
    const inputValue = repoValue + collateralValue + fraudValue + cleanupValue;

    const outputValue = repoValue + collateralValue + cleanupValue;
    const requiredValue = outputValue + BigInt(this.txFee) - inputValue;

    const selectedFeeBoxes = await this.selectFeeBoxes(requiredValue);

    // Create input boxes
    const inputBoxes = [
      this.repoData.box,
      this.collateralBoxData.box,
      this.fraudBoxData.box,
      this.cleanupBox,
      ...selectedFeeBoxes,
    ];

    // Create output boxes (repo, collateral, cleanup)
    const newRepoBox = this.createRepoBox();
    const newCollateralBox = this.createCollateralBox();
    const newCleanupBox = this.createCleanupBox();
    const outputs = [newRepoBox, newCollateralBox, newCleanupBox];

    // Create change box with remaining assets (including slashed RSN from fraud box)
    const changeBox = createChangeBox(
      inputBoxes,
      outputs,
      this.changeAddress,
      this.txFee,
      this.height,
    );

    const outputBoxes = [...outputs, changeBox];

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
