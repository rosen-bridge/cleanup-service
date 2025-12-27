import { vi } from 'vitest';
import * as ergoLib from 'ergo-lib-wasm-nodejs';

const signTxMocks = vi.hoisted(() => {
  const signTxMock = vi.fn(
    async (_nodeUrl: string, _mnemonic: string, unsignedTx: ergoLib.UnsignedTransaction) => {
      return ergoLib.Transaction.from_unsigned_tx(unsignedTx, [new Uint8Array()]);
    },
  );
  return { signTxMock };
});

vi.mock('../../src/services/ergo/signTx', () => ({
  signTx: signTxMocks.signTxMock,
}));


