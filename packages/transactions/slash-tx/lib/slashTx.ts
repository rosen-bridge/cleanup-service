import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import {
  ErgoBoxSelection,
  ErgoChangeBoxBuilder,
} from '@rosen-bridge/ergo-box-selection';
import { getTokenAmount } from './utils';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { RWTRepoBuilder } from '@rosen-bridge/rwt-repo';
import { CollateralBox } from '@rosen-bridge/collateral';
import { getWidFromR4Bytes } from './utils';
import { RWTRepoData } from './types';

/**
 * SlashTx class handles slashing transactions for fraudulent watchers
 * This is the TypeScript equivalent of the Scala slashFraud transaction
 */
export class SlashTx {
  private static _instance?: SlashTx;

  private constructor(
    private minBoxValue: bigint,
    private txFee: string,
    private logger?: AbstractLogger,
  ) {}

  /**
   * Initializes the singleton instance of SlashTx
   */
  static init = (
    minBoxValue: bigint,
    txFee: string,
    logger?: AbstractLogger,
  ): void => {
    if (SlashTx._instance !== undefined) {
      return;
    }
    SlashTx._instance = new SlashTx(minBoxValue, txFee, logger);
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
    return new SlashTxBuilder(this.minBoxValue, this.txFee, this.logger);
  };
}

/**
 * Builder class for creating slash transactions
 */
export class SlashTxBuilder {
  private fraudBox: ergoLib.ErgoBox;
  private collateralBox: ergoLib.ErgoBox;
  private repoData: RWTRepoData;
  private cleanupBox: ergoLib.ErgoBox;
  private height: number;
  private feeBoxes: ergoLib.ErgoBox[];
  private changeAddress: string;
  private collateralWid: string;
  private fraudWid: string;

  constructor(
    private minBoxValue: bigint,
    private txFee: string,
    private logger?: AbstractLogger,
  ) {}

  setFraudBox = (fraudBox: ergoLib.ErgoBox): SlashTxBuilder => {
    this.fraudBox = fraudBox;
    this.fraudWid = getWidFromR4Bytes(fraudBox);
    this.logger?.debug(`Fraud box set with id ${fraudBox.box_id().to_str()}`);
    return this;
  };

  setCollateralBox = (collateralBox: ergoLib.ErgoBox): SlashTxBuilder => {
    this.collateralBox = collateralBox;
    this.collateralWid = getWidFromR4Bytes(collateralBox);
    this.logger?.debug(
      `Collateral box set with id ${collateralBox.box_id().to_str()}`,
    );
    return this;
  };

  setRepoData = (repoData: RWTRepoData): SlashTxBuilder => {
    this.repoData = repoData;
    this.logger?.debug(`Repo data set`);
    return this;
  };

  setCleanupBox = (cleanupBox: ergoLib.ErgoBox): SlashTxBuilder => {
    this.cleanupBox = cleanupBox;
    this.logger?.debug(
      `Cleanup box set with id=${cleanupBox.box_id().to_str()}`,
    );
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
    const slashedRwtCount = getTokenAmount(
      this.fraudBox,
      this.repoData.rwtTokenId,
    );
    const collateralRsnAmount = getTokenAmount(
      this.collateralBox,
      this.repoData.rsnTokenId,
    );

    if (collateralRsnAmount < slashedRwtCount) {
      throw new Error(
        `Impossible case: fraud RWT (${slashedRwtCount}) is more than collateral RSN (${collateralRsnAmount}) for watcher ${this.fraudWid}`,
      );
    }

    if (this.fraudWid !== this.collateralWid) {
      throw new Error(
        `WID mismatch: fraud box WID (${this.fraudWid}) != collateral box WID (${this.collateralWid})`,
      );
    }
  };

  /**
   * Creates the new repo box with updated RWT and RSN counts
   */
  private createRepoBox = (): ergoLib.ErgoBoxCandidate => {
    const repoBox = this.repoData.box;
    const chainIdConstant = repoBox.register_value(
      ergoLib.NonMandatoryRegisterId.R4,
    );
    if (!chainIdConstant) {
      throw new Error('RWT repo box is missing R4 register');
    }
    const chainId = Buffer.from(chainIdConstant.to_byte_array()).toString();

    const watcherCountConstant = repoBox.register_value(
      ergoLib.NonMandatoryRegisterId.R5,
    );
    if (!watcherCountConstant) {
      throw new Error('RWT repo box is missing R5 register');
    }
    const watcherCount = Number(watcherCountConstant.to_i64().to_str());

    const repoBuilder = new RWTRepoBuilder(
      repoBox.ergo_tree().to_base16_bytes(),
      this.repoData.repoNFT,
      this.repoData.awcTokenId,
      this.getTokenAmount(repoBox, this.repoData.awcTokenId),
      this.repoData.rwtTokenId,
      this.getTokenAmount(repoBox, this.repoData.rwtTokenId),
      this.repoData.rsnTokenId,
      this.getTokenAmount(repoBox, this.repoData.rsnTokenId),
      chainId,
      watcherCount,
      this.logger,
    );

    const rwtAmount = getTokenAmount(this.fraudBox, this.repoData.rwtTokenId);
    repoBuilder.returnPermits(rwtAmount);
    repoBuilder.setValue(BigInt(repoBox.value().as_i64().to_str()));
    repoBuilder.setHeight(this.height);

    return repoBuilder.build();
  };

  /**
   * Creates the updated collateral box with reduced RSN token and R5
   */
  private createCollateralBox = (): ergoLib.ErgoBoxCandidate => {
    const slashedRwtCount = getTokenAmount(
      this.fraudBox,
      this.repoData.rwtTokenId,
    );
    const collateral = new CollateralBox(this.collateralBox, this.logger);
    const collateralBuilder = collateral.toBuilder();
    collateralBuilder.unlockRsn(slashedRwtCount);
    collateralBuilder.setHeight(this.height);
    return collateralBuilder.build();
  };

  /**
   * Creates the new cleanup box preserving input cleanup box
   */
  private createCleanupBox = (
    slashedRwtCount: bigint,
    rsnTokenId: string,
  ): ergoLib.ErgoBoxCandidate => {
    const boxBuilder = new ergoLib.ErgoBoxCandidateBuilder(
      ergoLib.BoxValue.from_i64(
        ergoLib.I64.from_str(this.cleanupBox.value().as_i64().to_str()),
      ),
      ergoLib.Contract.new(this.cleanupBox.ergo_tree()),
      this.height,
    );

    let rsnAmount = slashedRwtCount;
    for (let i = 0; i < this.cleanupBox.tokens().len(); i++) {
      const token = this.cleanupBox.tokens().get(i);
      if (token.id().to_str() === rsnTokenId) {
        rsnAmount += BigInt(token.amount().as_i64().to_str());
      } else {
        boxBuilder.add_token(token.id(), token.amount());
      }
    }
    boxBuilder.add_token(
      ergoLib.TokenId.from_str(rsnTokenId),
      ergoLib.TokenAmount.from_i64(ergoLib.I64.from_str(rsnAmount.toString())),
    );

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

    const selector = new ErgoBoxSelection(this.logger);
    const { covered, boxes } = await selector.getCoveringBoxes(
      { nativeToken: requiredValue, tokens: [] },
      [],
      new Map(),
      this.feeBoxes.values(),
    );

    if (!covered) {
      throw new Error(
        `Available fee boxes are not enough to cover required value=${requiredValue}`,
      );
    }

    this.logger?.debug(
      `Selected ${boxes.length} fee boxes: ${boxes.map((box) => box.box_id().to_str()).join(', ')}`,
    );

    return boxes;
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

    const fraudValue = BigInt(this.fraudBox.value().as_i64().to_str());
    const cleanupValue = BigInt(this.cleanupBox.value().as_i64().to_str());

    // Fraud input plus the cleanup box value can cover the fee.
    const requiredFee = BigInt(this.txFee) + this.minBoxValue - fraudValue;
    const selectedFeeBoxes = await this.selectFeeBoxes(requiredFee);

    // Create input boxes
    const inputBoxes = [
      this.repoData.box,
      this.collateralBox,
      this.fraudBox,
      this.cleanupBox,
      ...selectedFeeBoxes,
    ];

    // Create output boxes (repo, collateral, cleanup)
    const newRepoBox = this.createRepoBox();
    const newCollateralBox = this.createCollateralBox();
    const newCleanupBox = this.createCleanupBox(
      getTokenAmount(this.fraudBox, this.repoData.rwtTokenId),
      this.repoData.rsnTokenId,
    );
    const outputs = [newRepoBox, newCollateralBox, newCleanupBox];

    // Create change box with remaining assets (including slashed RSN from fraud box)
    const changeBoxes = ErgoChangeBoxBuilder.fromBoxes(
      this.changeAddress,
      inputBoxes,
      outputs,
      BigInt(this.txFee),
    ).build({ height: this.height });

    const outputBoxes = [...outputs, ...changeBoxes];

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
