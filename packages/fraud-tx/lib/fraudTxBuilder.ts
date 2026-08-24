import * as ergoLib from 'ergo-lib-wasm-nodejs';

import { AbstractLogger, DummyLogger } from '@rosen-bridge/abstract-logger';
import {
  ErgoBoxSelection,
  ErgoChangeBoxBuilder,
} from '@rosen-bridge/ergo-box-selection';

import { DEFAULT_MAX_TOKEN_COUNT } from './constants';
import { FraudTransactionBuildResult, TriggerEventData } from './types';

/**
 * Builder class for creating fraud transactions.
 */
export class FraudTxBuilder {
  private triggerEventData: TriggerEventData;
  private cleanerBox: ergoLib.ErgoBox;
  private height: number;
  private feeBoxes: ergoLib.ErgoBox[];

  constructor(
    private fraudAddress: string,
    private changeAddress: string,
    private rwtTokenId: string,
    private minBoxValue: bigint,
    private txFee: string,
    private logger: AbstractLogger = new DummyLogger(),
  ) {}

  /**
   * Sets the trigger event data containing watcher IDs and RWT amount.
   *
   * @param triggerEventData - Trigger event data
   * @returns This builder instance
   */
  setTriggerEventData = (
    triggerEventData: TriggerEventData,
  ): FraudTxBuilder => {
    this.triggerEventData = triggerEventData;
    this.logger.debug(
      `Trigger event data set with [${triggerEventData.wids.length}] watcher IDs`,
    );
    return this;
  };

  /**
   * Sets the cleaner box to be spent.
   *
   * @param cleanerBox - Cleaner box
   * @returns This builder instance
   */
  setCleanerBox = (cleanerBox: ergoLib.ErgoBox): FraudTxBuilder => {
    this.cleanerBox = cleanerBox;
    this.logger.debug(
      `Cleaner box set with id=${cleanerBox.box_id().to_str()}`,
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
  setCreationHeight = (height: number): FraudTxBuilder => {
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
  setFeeBoxes = (feeBoxes: ergoLib.ErgoBox[]): FraudTxBuilder => {
    this.feeBoxes = feeBoxes;
    this.logger.debug(`Fee boxes set: [${feeBoxes.length}] boxes available`);
    return this;
  };

  /**
   * Sets the change address for leftover assets.
   *
   * @param address - Change address in base58
   * @returns This builder instance
   */
  setChangeAddress = (address: string): FraudTxBuilder => {
    this.changeAddress = address;
    this.logger.debug(`Change address set to ${address}`);
    return this;
  };

  /**
   * Creates fraud boxes, one for each watcher ID in the trigger event.
   *
   * @returns Array of fraud box candidates
   */
  private createFraudBoxes = (): ergoLib.ErgoBoxCandidate[] => {
    const watcherCount = this.triggerEventData.wids.length;
    const rwtPerFraud = this.triggerEventData.rwtAmount / BigInt(watcherCount);
    const ergoPerFraud =
      BigInt(this.triggerEventData.box.value().as_i64().to_str()) /
      BigInt(watcherCount);

    this.logger.debug(
      `Creating [${watcherCount}] fraud boxes with [${rwtPerFraud}] RWT each`,
    );

    return this.triggerEventData.wids.map((wid: string) => {
      const boxBuilder = new ergoLib.ErgoBoxCandidateBuilder(
        ergoLib.BoxValue.from_i64(
          ergoLib.I64.from_str(ergoPerFraud.toString()),
        ),
        ergoLib.Contract.pay_to_address(
          ergoLib.Address.from_base58(this.fraudAddress),
        ),
        this.height,
      );

      // Add RWT token
      boxBuilder.add_token(
        ergoLib.TokenId.from_str(this.rwtTokenId),
        ergoLib.TokenAmount.from_i64(
          ergoLib.I64.from_str(rwtPerFraud.toString()),
        ),
      );

      const widBytes = Uint8Array.from(Buffer.from(wid, 'hex'));

      // Set R4 register with WID
      boxBuilder.set_register_value(
        4,
        ergoLib.Constant.from_byte_array(widBytes),
      );

      return boxBuilder.build();
    });
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
      this.minBoxValue,
      DEFAULT_MAX_TOKEN_COUNT,
      () => BigInt(this.txFee),
    );

    if (!covered) {
      throw new Error(
        `Available fee boxes are not enough to cover required value=${requiredValue}`,
      );
    }

    this.logger.debug(
      `Selected [${boxes.length}] fee boxes: ${boxes.map((box) => box.box_id().to_str()).join(', ')}`,
    );

    return boxes;
  };

  /**
   * Creates the new cleaner box preserving the input cleaner box's value and tokens.
   *
   * @returns New cleaner box candidate
   */
  private createCleanerBox = (): ergoLib.ErgoBoxCandidate => {
    const boxBuilder = new ergoLib.ErgoBoxCandidateBuilder(
      this.cleanerBox.value(),
      ergoLib.Contract.new(this.cleanerBox.ergo_tree()),
      this.height,
    );

    // Add all tokens from the input cleaner box
    for (let i = 0; i < this.cleanerBox.tokens().len(); i++) {
      const token = this.cleanerBox.tokens().get(i);
      boxBuilder.add_token(token.id(), token.amount());
    }

    return boxBuilder.build();
  };

  /**
   * Builds the unsigned fraud transaction.
   *
   * @returns Unsigned transaction and input boxes
   */
  build = async (): Promise<FraudTransactionBuildResult> => {
    // Calculate how much ERG we need
    const fraudBoxCount = this.triggerEventData.wids.length;
    const triggerBoxValue = BigInt(
      this.triggerEventData.box.value().as_i64().to_str(),
    );
    const requiredFee =
      BigInt(this.txFee) +
      this.minBoxValue * BigInt(fraudBoxCount) -
      triggerBoxValue;
    const selectedFeeBoxes = await this.selectFeeBoxes(requiredFee);

    // Create input boxes
    const inputBoxes = [
      this.triggerEventData.box,
      this.cleanerBox,
      ...selectedFeeBoxes,
    ];

    // Create output boxes (fraud boxes + cleaner box)
    const fraudBoxes = this.createFraudBoxes();
    const newCleanerBox = this.createCleanerBox();
    const outputs = [...fraudBoxes, newCleanerBox];

    // Create change box with remaining assets
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
      `Unsigned fraud transaction built with id=${unsignedTx.id().to_str()}`,
    );
    this.logger.debug(
      `Built unsigned fraud transaction: ${unsignedTx.to_json()}`,
    );

    return { unsignedTx, inputBoxes };
  };
}
