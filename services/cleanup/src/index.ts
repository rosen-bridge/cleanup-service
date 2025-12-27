import './bootstrap';

import { CallbackLoggerFactory } from '@rosen-bridge/callback-logger';
import { ServiceManager } from '@rosen-bridge/service-manager';
import * as ergoLib from 'ergo-lib-wasm-nodejs';

import { CleanupWorkflowService } from './services/cleanupWorkflowService';
import { BoxLookupService } from './services/boxLookupService';
import { DBService } from './services/dbService';
import { loadRosenContracts } from './services/ergo/contractsConfig';
import { mnemonicToAddress } from './services/ergo/keys';
import { ScannerService } from './services/scannerService';
import { TxPotService } from './services/txPotService';
import dataSource from './db/dataSource';
import { configs } from './config';

const logger = CallbackLoggerFactory.getInstance().getLogger(import.meta.url);

const main = async () => {
  const serviceManager = ServiceManager.setup();

  const contracts = loadRosenContracts(configs.workflow.contractsPath);
  const prefix =
    configs.workflow.ergoNetwork === 'mainnet'
      ? ergoLib.NetworkPrefix.Mainnet
      : ergoLib.NetworkPrefix.Testnet;
  const cleanupAddress = mnemonicToAddress(configs.workflow.cleanupMnemonic, prefix);
  const trackedAddresses = [
    contracts.addresses.RWTRepo,
    cleanupAddress,
  ];

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
  CleanupWorkflowService.init(logger);

  serviceManager.register(DBService.getInstance());
  serviceManager.register(ScannerService.getInstance());
  serviceManager.register(TxPotService.getInstance());
  serviceManager.register(BoxLookupService.getInstance());
  serviceManager.register(CleanupWorkflowService.getInstance());

  await serviceManager.start(CleanupWorkflowService.getInstance().getName());
};

main().catch((e) => {
  logger.error(e);
  process.exit(1);
});