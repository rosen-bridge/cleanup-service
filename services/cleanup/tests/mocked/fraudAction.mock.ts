import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { vi } from 'vitest';

const fraudTxMocks = vi.hoisted(() => {
  const builder = {
    setTriggerEventData: vi.fn().mockReturnThis(),
    setCleanerBox: vi.fn().mockReturnThis(),
    setCreationHeight: vi.fn().mockReturnThis(),
    setFeeBoxes: vi.fn().mockReturnThis(),
    setChangeAddress: vi.fn().mockReturnThis(),
    build: vi.fn().mockResolvedValue({
      unsignedTx: {} as ergoLib.UnsignedTransaction,
      inputBoxes: [] as ergoLib.ErgoBox[],
    }),
  };
  const instance = {
    newBuilder: vi.fn(() => builder),
  };
  return {
    init: vi.fn(),
    getInstance: vi.fn(() => instance),
    builder,
  };
});

export const fraudTxInitMock = fraudTxMocks.init;
export const fraudTxGetInstanceMock = fraudTxMocks.getInstance;
export const fraudTxBuilderMock = fraudTxMocks.builder;

vi.mock('@rosen-bridge/fraud-tx', () => ({
  FraudTx: {
    init: fraudTxMocks.init,
    getInstance: fraudTxMocks.getInstance,
  },
}));
