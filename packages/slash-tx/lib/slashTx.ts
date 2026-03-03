import * as ergoLib from 'ergo-lib-wasm-nodejs';

import { AbstractLogger, DummyLogger } from '@rosen-bridge/abstract-logger';
import { CollateralBox } from '@rosen-bridge/collateral';
import {
  ErgoBoxSelection,
  ErgoChangeBoxBuilder,
} from '@rosen-bridge/ergo-box-selection';
import { RWTRepo } from '@rosen-bridge/rwt-repo';

import { getTokenAmount } from './utils';
import { getWidFromR4Bytes } from './utils';

/**
 * Singleton class for creating slash transactions for fraudulent watchers.
 */
export class SlashTx {
  private static _instance?: SlashTx;

  private constructor(
    private minBoxValue: bigint,
    private txFee: string,
    private logger?: AbstractLogger,
  ) {}

  /**
   * Initializes the singleton instance of SlashTx.
   *
   * @param minBoxValue - Minimum ERG value for output boxes
   * @param txFee - Transaction fee in nanoERG
   * @param logger - Optional logger instance
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
   * Returns the singleton instance of SlashTx.
   *
   * @returns SlashTx instance
   * @throws When instance is not initialized
   */
  static getInstance = (): SlashTx => {
    if (!this._instance) {
      throw new Error('SlashTx instance is not initialized yet');
    }
    return this._instance;
  };

  /**
   * Creates a new SlashTxBuilder instance.
   *
   * @returns New SlashTxBuilder
   */
  newBuilder = (): SlashTxBuilder => {
    return new SlashTxBuilder(this.minBoxValue, this.txFee, this.logger);
  };
}

/**
 * Builder class for creating slash transactions.
 */
export class SlashTxBuilder {
  private fraudBox: ergoLib.ErgoBox;
  private collateralBox: ergoLib.ErgoBox;
  private repoBox: ergoLib.ErgoBox;
  private rwtRepo: RWTRepo;
  private cleanupBox: ergoLib.ErgoBox;
  private height: number;
  private feeBoxes: ergoLib.ErgoBox[];
  private changeAddress: string;
  private collateralWid: string;
  private fraudWid: string;

  constructor(
    private minBoxValue: bigint,
    private txFee: string,
    private logger: AbstractLogger = new DummyLogger(),
  ) {}

  /**
   * Sets the fraud box to be slashed.
   *
   * @param fraudBox - Fraud box containing RWT tokens
   * @returns This builder instance
   */
  setFraudBox = (fraudBox: ergoLib.ErgoBox): SlashTxBuilder => {
    this.fraudBox = fraudBox;
    this.fraudWid = getWidFromR4Bytes(fraudBox);
    this.logger.debug(`Fraud box set with id ${fraudBox.box_id().to_str()}`);
    return this;
  };

  /**
   * Sets the collateral box of the watcher being slashed.
   *
   * @param collateralBox - Collateral box containing RSN tokens
   * @returns This builder instance
   */
  setCollateralBox = (collateralBox: ergoLib.ErgoBox): SlashTxBuilder => {
    this.collateralBox = collateralBox;
    this.collateralWid = getWidFromR4Bytes(collateralBox);
    this.logger.debug(
      `Collateral box set with id ${collateralBox.box_id().to_str()}`,
    );
    return this;
  };

  /**
   * Sets the RWT repository box.
   *
   * @param repoBox - RWT repository box
   * @returns This builder instance
   */
  setRepoBox = (repoBox: ergoLib.ErgoBox): SlashTxBuilder => {
    this.repoBox = repoBox;
    this.rwtRepo = new RWTRepo(repoBox, this.logger);
    this.logger.debug(`Repo box set with id ${repoBox.box_id().to_str()}`);
    return this;
  };

  /**
   * Sets the cleanup box that will receive slashed RSN.
   *
   * @param cleanupBox - Cleanup box
   * @returns This builder instance
   */
  setCleanupBox = (cleanupBox: ergoLib.ErgoBox): SlashTxBuilder => {
    this.cleanupBox = cleanupBox;
    this.logger.debug(
      `Cleanup box set with id=${cleanupBox.box_id().to_str()}`,
    );
    return this;
  };

  /**
   * Sets the creation height for output boxes.
   *
   * @param height - Block height
   * @returns This builder instance
   * @throws When height is not positive
   */
  setCreationHeight = (height: number): SlashTxBuilder => {
    if (height < 1) {
      throw new Error('Creation height must be a positive integer');
    }
    this.height = height;
    this.logger.debug(`Creation height set to ${height}`);
    return this;
  };

  /**
   * Sets the fee boxes available for covering transaction fees.
   *
   * @param feeBoxes - Array of fee boxes
   * @returns This builder instance
   */
  setFeeBoxes = (feeBoxes: ergoLib.ErgoBox[]): SlashTxBuilder => {
    this.feeBoxes = feeBoxes;
    this.logger.debug(`Fee boxes set: ${feeBoxes.length} boxes available`);
    return this;
  };

  /**
   * Sets the change address for leftover assets.
   *
   * @param address - Change address in base58
   * @returns This builder instance
   */
  setChangeAddress = (address: string): SlashTxBuilder => {
    this.changeAddress = address;
    this.logger.debug(`Change address set to ${address}`);
    return this;
  };

  /**
   * Validates that the slash is possible.
   *
   * @throws When collateral RSN is less than fraud RWT
   * @throws When fraud box WID does not match collateral box WID
   */
  private validateSlash = (): void => {
    const slashedRwtCount = getTokenAmount(
      this.fraudBox,
      this.rwtRepo.getRwtId(),
    );
    const collateralRsnAmount = getTokenAmount(
      this.collateralBox,
      this.rwtRepo.getRsnId(),
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
   * Creates the new repo box with updated RWT and RSN counts.
   *
   * @returns New repo box candidate
   */
  private createRepoBox = (): ergoLib.ErgoBoxCandidate => {
    const repoBuilder = this.rwtRepo.toBuilder();

    const rwtAmount = getTokenAmount(this.fraudBox, this.rwtRepo.getRwtId());
    repoBuilder.returnPermits(rwtAmount);
    repoBuilder.setValue(BigInt(this.repoBox.value().as_i64().to_str()));
    repoBuilder.setHeight(this.height);

    return repoBuilder.build();
  };

  /**
   * Creates the updated collateral box with reduced RSN token.
   *
   * @returns New collateral box candidate
   */
  private createCollateralBox = (): ergoLib.ErgoBoxCandidate => {
    const slashedRwtCount = getTokenAmount(
      this.fraudBox,
      this.rwtRepo.getRwtId(),
    );
    const collateral = new CollateralBox(this.collateralBox, this.logger);
    const collateralBuilder = collateral.toBuilder();
    collateralBuilder.unlockRsn(slashedRwtCount);
    collateralBuilder.setHeight(this.height);
    return collateralBuilder.build();
  };

  /**
   * Creates the new cleanup box with added slashed RSN tokens.
   *
   * @param slashedRwtCount - Amount of RWT being slashed
   * @param rsnTokenId - RSN token ID
   * @returns New cleanup box candidate
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
   * Selects fee boxes to cover the required value.
   *
   * @param requiredValue - Required ERG value to cover
   * @returns Selected fee boxes
   * @throws When available boxes cannot cover the required value
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

    this.logger.debug(
      `Selected ${boxes.length} fee boxes: ${boxes.map((box) => box.box_id().to_str()).join(', ')}`,
    );

    return boxes;
  };

  /**
   * Builds the unsigned slash transaction.
   *
   * @returns Unsigned transaction and input boxes
   */
  build = async (): Promise<{
    unsignedTx: ergoLib.UnsignedTransaction;
    inputBoxes: ergoLib.ErgoBox[];
  }> => {
    this.validateSlash();

    const fraudValue = BigInt(this.fraudBox.value().as_i64().to_str());

    // Fraud input plus the cleanup box value can cover the fee.
    const requiredFee = BigInt(this.txFee) + this.minBoxValue - fraudValue;
    const selectedFeeBoxes = await this.selectFeeBoxes(requiredFee);

    // Create input boxes
    const inputBoxes = [
      this.repoBox,
      this.collateralBox,
      this.fraudBox,
      this.cleanupBox,
      ...selectedFeeBoxes,
    ];

    // Create output boxes (repo, collateral, cleanup)
    const newRepoBox = this.createRepoBox();
    const newCollateralBox = this.createCollateralBox();
    const newCleanupBox = this.createCleanupBox(
      getTokenAmount(this.fraudBox, this.rwtRepo.getRwtId()),
      this.rwtRepo.getRsnId(),
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

    this.logger.info(
      `Unsigned slash transaction built with id=${unsignedTx.id().to_str()}`,
    );
    this.logger.debug(
      `Built unsigned slash transaction: ${unsignedTx.to_json()}`,
    );

    return { unsignedTx, inputBoxes };
  };
}
