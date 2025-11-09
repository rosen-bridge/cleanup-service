import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { selectErgoBoxes } from '@rosen-bridge/ergo-box-selection';
import JsonBigInt from '@rosen-bridge/json-bigint';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { hexToUint8Array, toErgoBoxProxyIterator } from './utils';

/**
 * Represents a trigger event box containing watcher IDs that need to be frauded
 */
export interface TriggerEventData {
  box: ergoLib.ErgoBox;
  wids: string[]; // Array of watcher ID hex strings
  rwtAmount: bigint; // Total RWT tokens in the event box
}

/**
 * FraudTx class handles the creation of fraud boxes from trigger event boxes
 * This is the TypeScript equivalent of the Scala generateFrauds transaction
 */
export class FraudTx {
  private static _instance?: FraudTx;

  private constructor(
    private fraudAddress: string,
    private cleanerAddress: string,
    private rwtTokenId: string,
    private minBoxValue: bigint,
    private txFee: string,
    private logger?: AbstractLogger,
  ) {}

  /**
   * Initializes the singleton instance of FraudTx
   */
  static init = (
    fraudAddress: string,
    cleanerAddress: string,
    rwtTokenId: string,
    minBoxValue: bigint,
    txFee: string,
    logger?: AbstractLogger,
  ): void => {
    if (FraudTx._instance !== undefined) {
      return;
    }
    FraudTx._instance = new FraudTx(
      fraudAddress,
      cleanerAddress,
      rwtTokenId,
      minBoxValue,
      txFee,
      logger,
    );
  };

  /**
   * Returns the singleton instance of FraudTx
   */
  static getInstance = (): FraudTx => {
    if (!this._instance) {
      throw new Error('FraudTx instance is not initialized yet');
    }
    return this._instance;
  };

  /**
   * Creates a new FraudTxBuilder instance
   */
  newBuilder = (): FraudTxBuilder => {
    return new FraudTxBuilder(
      this.fraudAddress,
      this.cleanerAddress,
      this.rwtTokenId,
      this.minBoxValue,
      this.txFee,
      this.logger,
    );
  };
}

/**
 * Builder class for creating fraud transactions
 * Equivalent to Scala's generateFrauds method
 */
export class FraudTxBuilder {
  private triggerEventData: TriggerEventData;
  private cleanerBox: ergoLib.ErgoBox;
  private height: number;
  private feeBoxes: ergoLib.ErgoBox[];
  private changeAddress: string;

  constructor(
    private fraudAddress: string,
    private defaultChangeAddress: string,
    private rwtTokenId: string,
    private minBoxValue: bigint,
    private txFee: string,
    private logger?: AbstractLogger,
  ) {
    this.changeAddress = defaultChangeAddress;
  }

  /**
   * Sets trigger event data for the current instance
   */
  setTriggerEventData = (
    triggerEventData: TriggerEventData,
  ): FraudTxBuilder => {
    this.triggerEventData = triggerEventData;
    this.logger?.debug(
      `Trigger event data set with ${triggerEventData.wids.length} watcher IDs`,
    );
    return this;
  };

  /**
   * Sets cleaner box for the current instance
   */
  setCleanerBox = (cleanerBox: ergoLib.ErgoBox): FraudTxBuilder => {
    this.cleanerBox = cleanerBox;
    this.logger?.debug(
      `Cleaner box set with id=${cleanerBox.box_id().to_str()}`,
    );
    return this;
  };

  /**
   * Sets creation height for the current instance
   */
  setCreationHeight = (height: number): FraudTxBuilder => {
    if (height < 1) {
      throw new Error('Creation height must be a positive integer');
    }
    this.height = height;
    this.logger?.debug(`Creation height set to ${height}`);
    return this;
  };

  /**
   * Sets fee boxes for the current instance
   */
  setFeeBoxes = (feeBoxes: ergoLib.ErgoBox[]): FraudTxBuilder => {
    this.feeBoxes = feeBoxes;
    this.logger?.debug(`Fee boxes set: ${feeBoxes.length} boxes available`);
    return this;
  };

  /**
   * Sets change address for the current instance
   */
  setChangeAddress = (address: string): FraudTxBuilder => {
    this.changeAddress = address;
    this.logger?.debug(`Change address set to ${address}`);
    return this;
  };

  /**
   * Creates fraud boxes, one for each watcher ID in the trigger event
   */
  private createFraudBoxes = (): ergoLib.ErgoBoxCandidate[] => {
    const watcherCount = this.triggerEventData.wids.length;
    const rwtPerFraud = this.triggerEventData.rwtAmount / BigInt(watcherCount);

    this.logger?.debug(
      `Creating ${watcherCount} fraud boxes with ${rwtPerFraud} RWT each`,
    );

    return this.triggerEventData.wids.map((wid) => {
      const boxBuilder = new ergoLib.ErgoBoxCandidateBuilder(
        ergoLib.BoxValue.from_i64(
          ergoLib.I64.from_str(this.minBoxValue.toString()),
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

      // Set R4 register with WID (as Coll[Coll[Byte]])
      boxBuilder.set_register_value(
        4,
        ergoLib.Constant.from_coll_coll_byte([hexToUint8Array(wid)]),
      );

      return boxBuilder.build();
    });
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
   * Creates the new cleaner box with remaining assets
   */
  private createCleanerBox = (
    feeBoxes: ergoLib.ErgoBox[],
  ): ergoLib.ErgoBoxCandidate => {
    // Calculate total ERG: trigger event box + cleaner box + fee boxes - tx fee
    let totalErg =
      BigInt(this.triggerEventData.box.value().as_i64().to_str()) +
      BigInt(this.cleanerBox.value().as_i64().to_str());

    // Aggregate tokens from trigger event box, cleaner box, and fee boxes
    const tokens = new Map<string, bigint>();

    // Add trigger event box tokens (excluding RWT which goes to fraud boxes)
    for (let i = 0; i < this.triggerEventData.box.tokens().len(); i++) {
      const token = this.triggerEventData.box.tokens().get(i);
      const tokenId = token.id().to_str();
      if (tokenId !== this.rwtTokenId) {
        const amount = BigInt(token.amount().as_i64().to_str());
        tokens.set(tokenId, (tokens.get(tokenId) || 0n) + amount);
      }
    }

    // Add cleaner box tokens
    for (let i = 0; i < this.cleanerBox.tokens().len(); i++) {
      const token = this.cleanerBox.tokens().get(i);
      const tokenId = token.id().to_str();
      const amount = BigInt(token.amount().as_i64().to_str());
      tokens.set(tokenId, (tokens.get(tokenId) || 0n) + amount);
    }

    // Add fee boxes assets
    for (const feeBox of feeBoxes) {
      totalErg += BigInt(feeBox.value().as_i64().to_str());

      for (let i = 0; i < feeBox.tokens().len(); i++) {
        const token = feeBox.tokens().get(i);
        const tokenId = token.id().to_str();
        const amount = BigInt(token.amount().as_i64().to_str());
        tokens.set(tokenId, (tokens.get(tokenId) || 0n) + amount);
      }
    }

    // Subtract fraud boxes value and tx fee
    const fraudBoxCount = this.triggerEventData.wids.length;
    const fraudBoxesValue = this.minBoxValue * BigInt(fraudBoxCount);
    totalErg = totalErg - fraudBoxesValue - BigInt(this.txFee);

    const boxBuilder = new ergoLib.ErgoBoxCandidateBuilder(
      ergoLib.BoxValue.from_i64(ergoLib.I64.from_str(totalErg.toString())),
      ergoLib.Contract.new(this.cleanerBox.ergo_tree()),
      this.height,
    );

    // Add all tokens to the new cleaner box
    tokens.forEach((amount, tokenId) => {
      boxBuilder.add_token(
        ergoLib.TokenId.from_str(tokenId),
        ergoLib.TokenAmount.from_i64(ergoLib.I64.from_str(amount.toString())),
      );
    });

    return boxBuilder.build();
  };

  /**
   * Builds the unsigned fraud transaction
   * spends trigger event box, cleaner box, and optional fee boxes
   * creates multiple fraud boxes (one per watcher) and a new cleaner box
   */
  build = async (): Promise<{
    unsignedTx: ergoLib.UnsignedTransaction;
    inputBoxes: ergoLib.ErgoBox[];
  }> => {
    // Calculate how much ERG we need
    const fraudBoxCount = this.triggerEventData.wids.length;
    const outputValue = this.minBoxValue * BigInt(fraudBoxCount + 1); // fraud boxes + cleaner box
    const inputValue =
      BigInt(this.triggerEventData.box.value().as_i64().to_str()) +
      BigInt(this.cleanerBox.value().as_i64().to_str());
    const requiredValue = outputValue - inputValue + BigInt(this.txFee);

    const selectedFeeBoxes = await this.selectFeeBoxes(requiredValue);

    // Create output boxes
    const fraudBoxes = this.createFraudBoxes();
    const newCleanerBox = this.createCleanerBox(selectedFeeBoxes);
    const outputBoxes = [...fraudBoxes, newCleanerBox];

    // Create input boxes
    const inputBoxes = [
      this.triggerEventData.box,
      this.cleanerBox,
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
      `Unsigned fraud transaction built with id=${unsignedTx.id().to_str()}`,
    );
    this.logger?.debug(
      `Built unsigned fraud transaction: ${unsignedTx.to_json()}`,
    );

    return { unsignedTx, inputBoxes };
  };
}
