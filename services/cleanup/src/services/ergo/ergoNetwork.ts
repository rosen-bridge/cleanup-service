import ergoNodeClientFactory, { BlockHeader } from '@rosen-clients/ergo-node';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import JsonBigInt from '@rosen-bridge/json-bigint';

export class ErgoNetwork {
  static getErgoStateContext = async (nodeUrl: string): Promise<ergoLib.ErgoStateContext> => {
    const client = ergoNodeClientFactory(nodeUrl);
    const lastHeaders = await client.blocks.getLastHeaders(10n);
    const headersJson = lastHeaders.map((h: BlockHeader) => JsonBigInt.stringify(h));
    const blockHeaders = ergoLib.BlockHeaders.from_json(headersJson);
    const preHeader = ergoLib.PreHeader.from_block_header(blockHeaders.get(0));
    return new ergoLib.ErgoStateContext(preHeader, blockHeaders);
  };
}


