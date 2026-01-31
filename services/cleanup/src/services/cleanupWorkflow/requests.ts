import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { Request } from '@ergo-raffle/box-lookup';

import { configs } from '../../config/config';
import { createCleanupRequest, createFraudBoxRequest, createRepoRequest, createTriggerEventRequest } from '../../utils/boxLookupUtils';
import { RosenContracts, RequestIds } from '../../types';
import { BoxLookupService } from '../boxLookupService';
import { ScannerService } from '../scannerService';

/**
 * Registers box-lookup requests for the cleanup workflow.
 *
 * @param params - Request configuration and handlers
 * @returns Assigned request IDs
 */
export const registerCleanupRequests = (params: {
  contracts: RosenContracts;
  cleanupAddress: string;
  onCleanupSuffice: Request['onSuffice'];
  onRepoSuffice: Request['onSuffice'];
  onTriggerEventSuffice: Request['onSuffice'];
  onFraudBoxSuffice: Request['onSuffice'];
}): RequestIds => {
  const {
    contracts,
    cleanupAddress,
    onCleanupSuffice,
    onRepoSuffice,
    onTriggerEventSuffice,
    onFraudBoxSuffice,
  } = params;

  const cleanup = BoxLookupService.getInstance().addRequest(
    createCleanupRequest(
      ergoLib.Address.from_base58(cleanupAddress).to_ergo_tree().to_base16_bytes(),
      BigInt(configs.workflow.txFee) + BigInt(configs.workflow.minCleanupValue),
      [{ tokenId: contracts.tokens.CleanupNFT, amount: 1n }],
      async () => ScannerService.getInstance().getUnspentBoxesByAddress(cleanupAddress),
      onCleanupSuffice,
    ),
  );

  const repo = BoxLookupService.getInstance().addRequest(
    createRepoRequest(
      ergoLib.Address.from_base58(contracts.addresses.RWTRepo).to_ergo_tree().to_base16_bytes(),
      undefined,
      [{ tokenId: contracts.tokens.RepoNFT, amount: 1n }],
      async () => ScannerService.getInstance().getUnspentBoxesByAddress(contracts.addresses.RWTRepo),
      onRepoSuffice,
    ),
  );

  const trigger = BoxLookupService.getInstance().addRequest(
    createTriggerEventRequest(
      ergoLib.Address.from_base58(contracts.addresses.WatcherTriggerEvent)
        .to_ergo_tree()
        .to_base16_bytes(),
      undefined,
      [{ tokenId: contracts.tokens.RWTId, amount: 1n }],
      async () => {
        const height = await ScannerService.getInstance().getCurrentHeight();
        const expiredBefore = height - contracts.cleanupConfirm;
        const confirmed = await ScannerService.getInstance().getUnspentTriggerBoxes();
        return confirmed.filter((b) => b.creationHeight <= expiredBefore);
      },
      onTriggerEventSuffice,
    ),
  );

  const fraud = BoxLookupService.getInstance().addRequest(
    createFraudBoxRequest(
      ergoLib.Address.from_base58(contracts.addresses.Fraud).to_ergo_tree().to_base16_bytes(),
      undefined,
      [{ tokenId: contracts.tokens.RWTId, amount: 1n }],
      async () => ScannerService.getInstance().getUnspentFraudBoxes(),
      onFraudBoxSuffice,
    ),
  );

  return {
    cleanup,
    repo,
    trigger,
    fraud,
  };
};
