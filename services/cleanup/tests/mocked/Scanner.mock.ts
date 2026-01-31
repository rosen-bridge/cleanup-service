import { vi } from 'vitest';

const scannerMocks = vi.hoisted(() => {
  const ergoNodeNetworkCtorMock = vi.fn(() => ({}));
  const ergoScannerUpdateMock = vi.fn().mockResolvedValue(undefined);
  const ergoScannerRegisterExtractorMock = vi.fn();
  const ergoScannerCtorMock = vi.fn(() => ({
    update: ergoScannerUpdateMock,
    registerExtractor: ergoScannerRegisterExtractorMock,
  }));
  return {
    ergoNodeNetworkCtorMock,
    ergoScannerUpdateMock,
    ergoScannerRegisterExtractorMock,
    ergoScannerCtorMock,
  };
});

export const ergoNodeNetworkCtorMock = scannerMocks.ergoNodeNetworkCtorMock;
export const ergoScannerUpdateMock = scannerMocks.ergoScannerUpdateMock;
export const ergoScannerRegisterExtractorMock = scannerMocks.ergoScannerRegisterExtractorMock;
export const ergoScannerCtorMock = scannerMocks.ergoScannerCtorMock;

vi.mock('@rosen-bridge/ergo-scanner', () => ({
  ErgoScanner: scannerMocks.ergoScannerCtorMock,
  ErgoNodeNetwork: scannerMocks.ergoNodeNetworkCtorMock,
}));


