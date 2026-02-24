import { vi } from 'vitest';

const txPotMocks = vi.hoisted(() => {
  const txPotRegisterChainMock = vi.fn();
  const txPotSetupMock = vi.fn(() => ({
    registerChain: txPotRegisterChainMock,
  }));

  const txPotUpdateMock = vi.fn().mockResolvedValue(undefined);
  const txPotAddTxMock = vi.fn().mockResolvedValue(undefined);
  const txPotGetTxsQueryMock = vi.fn().mockResolvedValue([]);
  const txPotRegisterCallbackMock = vi.fn();
  const txPotGetInstanceMock = vi.fn(() => ({
    update: txPotUpdateMock,
    addTx: txPotAddTxMock,
    getTxsQuery: txPotGetTxsQueryMock,
    registerChain: txPotRegisterChainMock,
    registerCallback: txPotRegisterCallbackMock,
  }));

  return {
    txPotRegisterChainMock,
    txPotSetupMock,
    txPotUpdateMock,
    txPotAddTxMock,
    txPotGetTxsQueryMock,
    txPotRegisterCallbackMock,
    txPotGetInstanceMock,
  };
});

export const txPotRegisterChainMock = txPotMocks.txPotRegisterChainMock;
export const txPotSetupMock = txPotMocks.txPotSetupMock;
export const txPotUpdateMock = txPotMocks.txPotUpdateMock;
export const txPotAddTxMock = txPotMocks.txPotAddTxMock;
export const txPotGetTxsQueryMock = txPotMocks.txPotGetTxsQueryMock;
export const txPotRegisterCallbackMock = txPotMocks.txPotRegisterCallbackMock;
export const txPotGetInstanceMock = txPotMocks.txPotGetInstanceMock;

vi.mock('@rosen-bridge/tx-pot', async (importOriginal) => {
  const actual = await importOriginal<typeof import('@rosen-bridge/tx-pot')>();
  return {
    ...actual,
    TxPot: {
      ...actual.TxPot,
      setup: txPotMocks.txPotSetupMock,
      getInstance: txPotMocks.txPotGetInstanceMock,
    },
  };
});
