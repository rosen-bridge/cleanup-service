import { AbstractPotChainManager, SigningStatus } from '@rosen-bridge/tx-pot';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import ErgoNodeNetwork from '../../network/ergoNodeNetwork';
import { configs } from '../../config/config';
import { AbstractLogger } from '@rosen-bridge/abstract-logger';

export class ErgoNetworkInterface extends AbstractPotChainManager {
  readonly network: ErgoNodeNetwork;

  constructor(private txRequiredConfirmations: number, logger?: AbstractLogger) {
    super();
    this.network = new ErgoNodeNetwork(configs.scanner.nodeUrl, logger);
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
    const txBytes = Uint8Array.from(Buffer.from(serializedTx, 'base64'));
    const parsedTx = ergoLib.Transaction.sigma_parse_bytes(txBytes);
    const inputs = parsedTx.inputs();
    for (let i = 0; i < inputs.len(); i++) {
      const inputBoxId = inputs.get(i).box_id().to_str();
      const isInputValid = await this.network.isBoxUnspentAndValid(inputBoxId);
      if (!isInputValid) return false;
    }
    return true;
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


