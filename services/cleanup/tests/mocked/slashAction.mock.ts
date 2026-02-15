import { vi } from 'vitest';
import * as ergoLib from 'ergo-lib-wasm-nodejs';

const slashTxMocks = vi.hoisted(() => {
  const builder = {
    setFraudBox: vi.fn().mockReturnThis(),
    setCollateralBox: vi.fn().mockReturnThis(),
    setRepoBox: vi.fn().mockReturnThis(),
    setCleanupBox: vi.fn().mockReturnThis(),
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

export const slashTxInitMock = slashTxMocks.init;
export const slashTxGetInstanceMock = slashTxMocks.getInstance;
export const slashTxBuilderMock = slashTxMocks.builder;

vi.mock('@rosen-bridge/slash-tx', () => ({
  SlashTx: {
    init: slashTxMocks.init,
    getInstance: slashTxMocks.getInstance,
  },
}));
