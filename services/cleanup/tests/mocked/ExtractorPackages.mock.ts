import { vi } from 'vitest';

vi.mock('@rosen-bridge/watcher-data-extractor', async (importOriginal) => {
  const actual =
    await importOriginal<typeof import('@rosen-bridge/watcher-data-extractor')>();
  return {
    ...actual,
    EventTriggerExtractor: vi.fn(() => ({})),
    CollateralExtractor: vi.fn(() => ({})),
    CommitmentExtractor: vi.fn(() => ({})),
  };
});

vi.mock('@rosen-bridge/fraud-extractor', async (importOriginal) => {
  const actual = await importOriginal<typeof import('@rosen-bridge/fraud-extractor')>();
  return {
    ...actual,
    FraudExtractor: vi.fn(() => ({})),
  };
});


