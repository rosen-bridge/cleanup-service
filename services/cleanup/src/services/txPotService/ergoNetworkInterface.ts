import { AbstractPotChainManager, SigningStatus } from '@rosen-bridge/tx-pot';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import ErgoNodeNetwork from '../../network/ergoNodeNetwork';
import { configs } from '../../config/config';

export class ErgoNetworkInterface extends AbstractPotChainManager {
  readonly network: ErgoNodeNetwork;

  constructor(private txRequiredConfirmations: number) {
    super();
    this.network = new ErgoNodeNetwork(configs.scanner.nodeUrl);
  }

  /**
   * Returns chain height.
   *
   * @returns Current height
   */
  getHeight = async (): Promise<number> => this.network.getHeight();

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
   * @param txId - Tx id
   * @returns Confirmations or -1 when unknown
   */
  getTxConfirmation = async (txId: string): Promise<number> =>
    this.network.getTxConfirmation(txId);

  /**
   * Checks whether a tx is valid for the given signing state.
   *
   * @param serializedTx - Serialized tx
   * @param _signingStatus - Signing status
   * @returns True when valid
   */
  isTxValid = async (
    serializedTx: string,
    _signingStatus: SigningStatus,
  ): Promise<boolean> => {
    // For now, just check if the transaction is not in mempool (basic validity check)
    const txBytes = Uint8Array.from(Buffer.from(serializedTx, 'base64'));
    const parsedTx = ergoLib.Transaction.sigma_parse_bytes(txBytes);
    const txId = parsedTx.id().to_str();
    return !(await this.network.isTxInMempool(txId));
  };

  /**
   * Submits a serialized transaction to the network.
   *
   * @param serializedTx - Serialized tx
   */
  submitTransaction = async (serializedTx: string): Promise<void> => {
    const txHex = Buffer.from(serializedTx, 'base64').toString('hex');
    await this.network.submitTransaction(txHex);
  };

  /**
   * Checks whether a tx id is currently in mempool.
   *
   * @param txId - Tx id
   * @returns True when in mempool
   */
  isTxInMempool = async (txId: string): Promise<boolean> =>
    this.network.isTxInMempool(txId);
}


