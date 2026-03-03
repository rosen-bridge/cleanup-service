import { AxiosError } from 'axios';
import * as ergoLib from 'ergo-lib-wasm-nodejs';

import { AbstractLogger, DummyLogger } from '@rosen-bridge/abstract-logger';
import JsonBigInt from '@rosen-bridge/json-bigint';
import ergoNodeClientFactory, { BlockHeader } from '@rosen-clients/ergo-node';

const TX_FETCHING_PAGE_SIZE = 50;

class ErgoNodeNetwork {
  private client: ReturnType<typeof ergoNodeClientFactory>;

  constructor(
    nodeUrl: string,
    private logger: AbstractLogger = new DummyLogger(),
  ) {
    this.client = ergoNodeClientFactory(nodeUrl);
  }

  /**
   * get current block height
   */
  public getHeight = async (): Promise<number> => {
    try {
      const nodeInfo = await this.client.getNodeInfo();
      this.logger.debug(
        `requested 'getNodeInfo'. res: ${JsonBigInt.stringify(nodeInfo)}`,
      );
      return Number(nodeInfo.fullHeight);
    } catch (error) {
      throw new Error(`Failed to get height from Ergo Node: ${error}`);
    }
  };

  /**
   * get confirmations of a tx or -1 if tx is not in the blockchain
   * @param txId - Transaction id
   */
  public getTxConfirmation = async (txId: string): Promise<number> => {
    try {
      const tx = await this.client.getTxById(txId);
      this.logger.debug(
        `requested 'getTxById' for txId [${txId}]. res: ${JsonBigInt.stringify(tx)}`,
      );
      return Number(tx.numConfirmations);
    } catch (error) {
      if (error instanceof AxiosError && error.response?.status === 404) {
        return -1;
      }
      throw new Error(
        `Failed to get tx confirmations from Ergo Node: ${error}`,
      );
    }
  };

  /**
   * get a mempool tx in each iteration until there are no more txs in it
   */
  private async *getOneMempoolTx() {
    let currentPage = 0;

    while (true) {
      const txsPage = await this.client.getUnconfirmedTransactions({
        offset: currentPage * TX_FETCHING_PAGE_SIZE,
        limit: TX_FETCHING_PAGE_SIZE,
      });

      if (txsPage.length) {
        yield* txsPage;
        currentPage += 1;
      } else {
        return;
      }
    }
  }

  /**
   * check if a specific transaction is in the mempool
   * @param txId - Transaction id
   */
  public isTxInMempool = async (txId: string): Promise<boolean> => {
    try {
      const txsIterator = this.getOneMempoolTx();

      for await (const tx of txsIterator) {
        if (tx.id === txId) {
          this.logger.debug(`Found transaction [${txId}] in mempool`);
          return true;
        }
      }

      return false;
    } catch (error) {
      throw new Error(
        `Failed to check if transaction is in mempool from Ergo Node: ${error}`,
      );
    }
  };

  /**
   * submit a transaction to the network
   * @param tx - Transaction in hex format
   */
  public submitTransaction = async (tx: string): Promise<void> => {
    try {
      await this.client.sendTransactionAsBytes(tx);
      const txBytes = Uint8Array.from(Buffer.from(tx, 'hex'));
      const parsedTx = ergoLib.Transaction.sigma_parse_bytes(txBytes);
      const txId = parsedTx.id().to_str();
      this.logger.info(`submitted transaction [${txId}] to Ergo Node`);
    } catch (error) {
      throw new Error(`Failed to submit transaction to Ergo Node: ${error}`);
    }
  };

  /**
   * check if a box is unspent and valid (that is, exists in the blockchain)
   * @param boxId - Box id
   */
  public isBoxUnspentAndValid = async (boxId: string): Promise<boolean> => {
    try {
      const box = await this.client.getBoxById(boxId);
      this.logger.debug(
        `requested 'getBoxById' for boxId [${boxId}]. res: ${JsonBigInt.stringify(box)}`,
      );

      return true;
    } catch (error) {
      if (error instanceof AxiosError && error.response?.status === 404) {
        return false;
      }
      throw new Error(
        `Failed to check if box is unspent and valid using Ergo Node: ${error}`,
      );
    }
  };

  /**
   * get current state context of blockchain using last ten blocks
   */
  public getErgoStateContext = async (): Promise<ergoLib.ErgoStateContext> => {
    const lastHeaders = await this.client.getLastHeaders(10);
    const headersJson = lastHeaders.map((h: BlockHeader) =>
      JsonBigInt.stringify(h),
    );
    const blockHeaders = ergoLib.BlockHeaders.from_json(headersJson);
    const preHeader = ergoLib.PreHeader.from_block_header(blockHeaders.get(0));
    return new ergoLib.ErgoStateContext(preHeader, blockHeaders);
  };
}

export default ErgoNodeNetwork;
