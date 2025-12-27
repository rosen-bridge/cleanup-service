import { vi } from 'vitest';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { dummyUnsignedTx, mkCandidate } from '../utils/testUtils';
import { workflowCleanupAddress, workflowCleanupNftTokenId, workflowRepoNftTokenId } from '../testData';


const fraudBuildMock = vi.fn().mockResolvedValue({
  unsignedTx: dummyUnsignedTx([
    mkCandidate(workflowCleanupAddress, workflowCleanupNftTokenId),
    mkCandidate(workflowCleanupAddress),
  ]),
  inputBoxes: [],
});

const slashBuildMock = vi.fn().mockResolvedValue({
  unsignedTx: dummyUnsignedTx([
    mkCandidate(workflowCleanupAddress, workflowCleanupNftTokenId),
    mkCandidate(workflowCleanupAddress, workflowRepoNftTokenId),
    mkCandidate(workflowCleanupAddress),
  ]),
  inputBoxes: [],
});

/**
 * Creates a minimal chainable builder mock with the given setter names.
 * Each setter returns the builder itself, and build() uses the provided mock.
 */
const mkChainableBuilder = (setters: string[], build: typeof vi.fn) => {
  const builder: Record<string, unknown> = {};
  for (const name of setters) {
    builder[name] = () => builder;
  }
  builder.build = build;
  return builder;
};

vi.mock('@rosen-bridge/fraud-tx', async (importOriginal) => {
  const actual = await importOriginal<typeof import('@rosen-bridge/fraud-tx')>();
  return {
    ...actual,
    FraudTx: {
      ...actual.FraudTx,
      init: vi.fn(),
      getInstance: () => ({
        newBuilder: () =>
          mkChainableBuilder(
            [
              'setTriggerEventData',
              'setCleanerBox',
              'setCreationHeight',
              'setFeeBoxes',
              'setChangeAddress',
            ],
            fraudBuildMock,
          ),
      }),
    },
  };
});

vi.mock('@rosen-bridge/slash-tx', async (importOriginal) => {
  const actual = await importOriginal<typeof import('@rosen-bridge/slash-tx')>();
  return {
    ...actual,
    SlashTx: {
      ...actual.SlashTx,
      init: vi.fn(),
      getInstance: () => ({
        newBuilder: () =>
          mkChainableBuilder(
            [
              'setFraudBoxData',
              'setCollateralBoxData',
              'setRepoData',
              'setCleanupBox',
              'setCreationHeight',
              'setFeeBoxes',
              'setChangeAddress',
            ],
            slashBuildMock,
          ),
      }),
    },
  };
});


