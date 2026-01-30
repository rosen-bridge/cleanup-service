import { afterEach, describe, expect, it, vi } from 'vitest';

import { ScannerService } from '../src/services/scannerService';
import { mockNodeUrl } from './testData';
import {
  ergoNodeNetworkCtorMock,
  ergoScannerCtorMock,
  ergoScannerRegisterExtractorMock,
  ergoScannerUpdateMock,
} from './mocked/Scanner.mock';
import { initServices } from './utils/testUtils';

describe('scannerService', () => {

  describe('startService', () => {
    /**
     * @target should schedule periodic scanner updates
     * @dependencies
     * - DBService started
     * @scenario
     * - Start scanner service
     * - Advance timers
     * @expected
     * - scanner.update called at least once
     */
    it('should schedule periodic scanner updates', async () => {
      vi.useFakeTimers();
      // arrange
      const ds = await initServices(1);

      // act
      const ok = await ScannerService.getInstance().startService();
      vi.advanceTimersByTime(1100);
      await vi.runOnlyPendingTimersAsync();

      // assert
      expect(ok).toBe(true);
      expect(ergoNodeNetworkCtorMock).toHaveBeenCalledWith(mockNodeUrl);
      expect(ergoScannerCtorMock).toHaveBeenCalled();
      expect(ergoScannerRegisterExtractorMock).toHaveBeenCalledTimes(4);
      expect(ergoScannerUpdateMock.mock.calls.length).toBeGreaterThanOrEqual(2);

      await ds.destroy();
    });
  });

  afterEach(() => {
    vi.clearAllMocks();
    vi.useRealTimers();
  });
});