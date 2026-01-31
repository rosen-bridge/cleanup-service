import { vi } from 'vitest';

const boxLookupMocks = vi.hoisted(() => {
  const boxLookupServiceRequestsMock = vi.fn().mockResolvedValue(undefined);
  const boxLookupRegisterRequestMock = vi.fn(
    (_: import('@ergo-raffle/box-lookup').Request) => 1,
  );
  const boxLookupUnregisterRequestMock = vi.fn();
  const boxLookupCtorMock = vi.fn(() => ({
    serveRequests: boxLookupServiceRequestsMock,
    registerRequest: boxLookupRegisterRequestMock,
    unregisterRequest: boxLookupUnregisterRequestMock,
  }));
  return {
    boxLookupServiceRequestsMock,
    boxLookupRegisterRequestMock,
    boxLookupUnregisterRequestMock,
    boxLookupCtorMock,
  };
});

export const boxLookupServiceRequestsMock = boxLookupMocks.boxLookupServiceRequestsMock;
export const boxLookupRegisterRequestMock = boxLookupMocks.boxLookupRegisterRequestMock;
export const boxLookupUnregisterRequestMock = boxLookupMocks.boxLookupUnregisterRequestMock;
export const boxLookupCtorMock = boxLookupMocks.boxLookupCtorMock;

vi.mock('@ergo-raffle/box-lookup', async (importOriginal) => {
  const actual = await importOriginal<typeof import('@ergo-raffle/box-lookup')>();
  return {
    ...actual,
    BoxLookup: boxLookupMocks.boxLookupCtorMock,
  };
});


