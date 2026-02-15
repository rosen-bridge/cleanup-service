import { vi } from 'vitest';
import { OutputBox } from '@ergo-raffle/box-lookup';
import * as ergoLib from 'ergo-lib-wasm-nodejs';

const cleanupUtilsMocks = vi.hoisted(() => ({
  outputBoxToErgoBox: vi.fn(
    (box: OutputBox) => box as unknown as ergoLib.ErgoBox,
  ),
  getCommitmentCountFromR7: vi.fn(() => 2),
  getWidListDigestFromR4: vi.fn(() => 'digest'),
  getTokenAmount: vi.fn(() => 1000n),
  hasToken: vi.fn((box: OutputBox, tokenId: string) =>
    (box.assets ?? []).some((asset) => asset.tokenId === tokenId),
  ),
  txToOutputs: vi.fn(
    (tx: unknown) => (tx as { __outputs?: OutputBox[] }).__outputs ?? [],
  ),
}));

export const outputBoxToErgoBoxMock = cleanupUtilsMocks.outputBoxToErgoBox;
export const getCommitmentCountFromR7Mock =
  cleanupUtilsMocks.getCommitmentCountFromR7;
export const getWidListDigestFromR4Mock =
  cleanupUtilsMocks.getWidListDigestFromR4;
export const getTokenAmountMock = cleanupUtilsMocks.getTokenAmount;
export const hasTokenMock = cleanupUtilsMocks.hasToken;
export const txToOutputsMock = cleanupUtilsMocks.txToOutputs;

vi.mock('../../src/utils/cleanupUtils', () => cleanupUtilsMocks);
