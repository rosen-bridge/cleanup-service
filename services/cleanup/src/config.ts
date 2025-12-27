import config from 'config';

import { BoxLookupConfig } from './types/boxLookupConfig';
import { DatabaseConfig } from './types/db';
import { ScannerConfig } from './types/scanner';
import { TxPotConfig } from './types/txpot';
import { WorkflowConfig } from './types/workflow';

export interface CleanupServiceConfig {
  readonly intervals: {
    readonly workflow: number; // seconds
  };
  readonly workflow: WorkflowConfig;
  readonly database: DatabaseConfig;
  readonly txpot: TxPotConfig;
  readonly scanner: ScannerConfig;
  readonly boxLookup: BoxLookupConfig;
}

const getBigInt = (key: string): bigint => {
  const v = config.get<string | number>(key);
  return BigInt(String(v));
};

const getRequiredString = (key: string): string => {
  const v = config.get<string>(key);
  if (!v || v.trim() === '') throw new Error(`${key} is not set`);
  return v;
};

const getOptionalNumber = (key: string): number | undefined => {
  if (!config.has(key)) return undefined;
  const v = config.get<string | number>(key);
  if (v === '' || v === null) return undefined;
  return Number(v);
};

export const configs: CleanupServiceConfig = {
  intervals: {
    workflow: config.get<number>('intervals.workflow'),
  },
  workflow: {
    contractsPath: getRequiredString('workflow.contractsPath'),
    ergoNetwork: config.get<'mainnet' | 'testnet'>('workflow.ergoNetwork'),
    cleanupMnemonic: getRequiredString('workflow.cleanupMnemonic'),
    minBoxValue: getBigInt('workflow.minBoxValue'),
    txFee: config.get<string>('workflow.txFee'),
  },
  database: {
    type: config.get<'sqlite' | 'postgres'>('database.type'),
    path: config.get<string>('database.path'),
    host: config.get<string>('database.host'),
    port: getOptionalNumber('database.port'),
    user: config.get<string>('database.user'),
    password: config.get<string>('database.password'),
    name: config.get<string>('database.name'),
  },
  txpot: {
    updateInterval: config.get<number>('txpot.updateInterval'),
    txRequiredConfirmations: config.get<number>('txpot.txRequiredConfirmations'),
  },
  scanner: {
    updateInterval: config.get<number>('scanner.updateInterval'),
    nodeUrl: getRequiredString('scanner.nodeUrl'),
    explorerUrl: getRequiredString('scanner.explorerUrl'),
    initialHeight: config.get<number>('scanner.initialHeight'),
  },
  boxLookup: {
    updateInterval: config.get<number>('boxLookup.updateInterval'),
    nodeUrl: getRequiredString('boxLookup.nodeUrl'),
  },
};


