import JsonBigInt from '@rosen-bridge/json-bigint';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { vi } from 'vitest';

import { last10BlockHeaders } from '../testData';

const ergoNodeNetworkMocks = vi.hoisted(() => {
  const getHeightMock = vi.fn().mockResolvedValue(1000000);
  const getTxConfirmationMock = vi.fn().mockResolvedValue(10);
  const isTxInMempoolMock = vi.fn().mockResolvedValue(false);
  const submitTransactionMock = vi.fn().mockResolvedValue(undefined);
  const isBoxUnspentAndValidMock = vi.fn().mockResolvedValue(true);

  const getErgoStateContextMock = vi.fn().mockImplementation(async () => {
    const headersJson = last10BlockHeaders.map((h) => JsonBigInt.stringify(h));
    const blockHeaders = ergoLib.BlockHeaders.from_json(headersJson);
    const preHeader = ergoLib.PreHeader.from_block_header(blockHeaders.get(0));
    return new ergoLib.ErgoStateContext(preHeader, blockHeaders);
  });

  const ergoNodeNetworkCtorMock = vi.fn(() => ({
    getHeight: getHeightMock,
    getTxConfirmation: getTxConfirmationMock,
    isTxInMempool: isTxInMempoolMock,
    submitTransaction: submitTransactionMock,
    isBoxUnspentAndValid: isBoxUnspentAndValidMock,
    getErgoStateContext: getErgoStateContextMock,
  }));

  return {
    getHeightMock,
    getTxConfirmationMock,
    isTxInMempoolMock,
    submitTransactionMock,
    isBoxUnspentAndValidMock,
    getErgoStateContextMock,
    ergoNodeNetworkCtorMock,
  };
});

export const getHeightMock = ergoNodeNetworkMocks.getHeightMock;
export const getTxConfirmationMock = ergoNodeNetworkMocks.getTxConfirmationMock;
export const isTxInMempoolMock = ergoNodeNetworkMocks.isTxInMempoolMock;
export const submitTransactionMock = ergoNodeNetworkMocks.submitTransactionMock;
export const isBoxUnspentAndValidMock =
  ergoNodeNetworkMocks.isBoxUnspentAndValidMock;
export const getErgoStateContextMock =
  ergoNodeNetworkMocks.getErgoStateContextMock;
export const ergoNodeNetworkCtorMock =
  ergoNodeNetworkMocks.ergoNodeNetworkCtorMock;

vi.mock('../../src/network/ergoNodeNetwork', () => ({
  default: ergoNodeNetworkMocks.ergoNodeNetworkCtorMock,
}));
