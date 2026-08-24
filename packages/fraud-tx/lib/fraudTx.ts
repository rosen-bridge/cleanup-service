import { AbstractLogger } from '@rosen-bridge/abstract-logger';

import { FraudTxBuilder } from './fraudTxBuilder';

/**
 * Singleton class for creating fraud transactions from trigger event boxes.
 */
export class FraudTx {
  /**
   * Creates a new FraudTxBuilder instance.
   *
   * @param fraudAddress - Address for fraud output boxes
   * @param cleanerAddress - Address of the cleaner box
   * @param rwtTokenId - RWT token ID
   * @param minBoxValue - Minimum ERG value for output boxes
   * @param txFee - Transaction fee in nanoERG
   * @param logger - Optional logger instance
   */
  constructor(
    private fraudAddress: string,
    private cleanerAddress: string,
    private rwtTokenId: string,
    private minBoxValue: bigint,
    private txFee: string,
    private logger?: AbstractLogger,
  ) {}

  /**
   * Creates a new FraudTxBuilder instance.
   *
   * @returns New FraudTxBuilder
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
