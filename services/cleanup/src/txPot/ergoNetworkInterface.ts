import { AbstractPotChainManager, SigningStatus } from '@rosen-bridge/tx-pot';

export class ErgoNetworkInterface extends AbstractPotChainManager {
  constructor(private txRequiredConfirmations: number) {
    super();
  }

  /**
   * Returns chain height.
   *
   * @returns Current height
   */
  getHeight = async (): Promise<number> => 0;

  /**
   * Returns required confirmations for a tx type.
   *
   * @param _txType - Tx type
   * @returns Required confirmations
   */
  getTxRequiredConfirmation = (_txType: string): number =>
    this.txRequiredConfirmations;

  /**
   * Returns confirmations for a tx id.
   *
   * @param _txId - Tx id
   * @returns Confirmations or -1 when unknown
   */
  getTxConfirmation = async (_txId: string): Promise<number> => -1;

  /**
   * Checks whether a tx is valid for the given signing state.
   *
   * @param _serializedTx - Serialized tx
   * @param _signingStatus - Signing status
   * @returns True when valid
   */
  isTxValid = async (
    _serializedTx: string,
    _signingStatus: SigningStatus,
  ): Promise<boolean> => true;

  /**
   * Submits a serialized transaction to the network.
   *
   * @param _serializedTx - Serialized tx
   */
  submitTransaction = async (_serializedTx: string): Promise<void> => {
    // TODO: implement actual submit via node/explorer client
    return;
  };

  /**
   * Checks whether a tx id is currently in mempool.
   *
   * @param _txId - Tx id
   * @returns True when in mempool
   */
  isTxInMempool = async (_txId: string): Promise<boolean> => false;
}


