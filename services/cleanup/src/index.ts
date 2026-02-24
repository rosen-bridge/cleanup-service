import './bootstrap';

import { ServiceManager } from '@rosen-bridge/service-manager';
import * as ergoLib from 'ergo-lib-wasm-nodejs';

import { CleanupService } from './services/cleanupService';
import { BoxLookupService } from './services/boxLookupService';
import { DBService } from './services/dbService';
import { loadRosenContracts } from './config/contractsConfig';
import { mnemonicToAddress } from './utils/ergoUtils';
import { ScannerService } from './services/scannerService';
import { TxPotService } from './services/txPotService/txPotService';
import dataSource from './db/dataSource';
import { configs } from './config/config';
import { getLogger } from './config/loggerConfig';

const logger = getLogger(import.meta.url);

const main = async () => {
  const serviceManager = ServiceManager.setup();

  const contracts = loadRosenContracts(configs.workflow.contractsPath);
  const prefix =
    configs.workflow.ergoNetwork === 'mainnet'
      ? ergoLib.NetworkPrefix.Mainnet
      : ergoLib.NetworkPrefix.Testnet;
  const cleanupAddress = mnemonicToAddress(
    configs.workflow.cleanupMnemonic,
    prefix,
  );
  const trackedAddresses = [contracts.addresses.RWTRepo, cleanupAddress];

  DBService.init(dataSource, logger);
  ScannerService.init(
    configs.scanner.updateInterval,
    configs.scanner.nodeUrl,
    configs.scanner.explorerUrl,
    configs.scanner.initialHeight,
    prefix,
    contracts,
    trackedAddresses,
    logger,
  );
  TxPotService.init(
    configs.txpot.updateInterval,
    dataSource,
    configs.txpot.txRequiredConfirmations,
    logger,
  );
  BoxLookupService.init(
    configs.boxLookup.updateInterval,
    configs.boxLookup.nodeUrl,
    logger,
  );
  CleanupService.init(logger);

  serviceManager.register(DBService.getInstance());
  serviceManager.register(ScannerService.getInstance());
  serviceManager.register(TxPotService.getInstance());
  serviceManager.register(BoxLookupService.getInstance());
  serviceManager.register(CleanupService.getInstance());

  await serviceManager.start(CleanupService.getInstance().getName());
};

main().catch((e) => {
  logger.error(e);
  process.exit(1);
});
