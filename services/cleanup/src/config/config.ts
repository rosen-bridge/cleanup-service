import config from 'config';

import { BoxLookup, Database, Logs, Scanner, Txpot, Workflow } from '../types';

export interface CleanupServiceConfig {
  readonly intervals: {
    readonly workflow: number; // seconds
  };
  readonly workflow: Workflow;
  readonly database: Database;
  readonly txpot: Txpot;
  readonly scanner: Scanner;
  readonly boxLookup: BoxLookup;
  readonly logs: Logs[];
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

const getOptionalString = (key: string): string | undefined => {
  if (!config.has(key)) return undefined;
  const v = config.get<string>(key);
  if (!v || v.trim() === '') return undefined;
  return v;
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
    minCleanupValue: getBigInt('workflow.minCleanupValue'),
    txFee: config.get<string>('workflow.txFee'),
  },
  database: {
    type: config.get<'sqlite' | 'postgres'>('database.type'),
    path: getOptionalString('database.path'),
    host: getOptionalString('database.host'),
    port: getOptionalNumber('database.port'),
    user: getOptionalString('database.user'),
    password: getOptionalString('database.password'),
    name: getOptionalString('database.name'),
  },
  txpot: {
    updateInterval: config.get<number>('txpot.updateInterval'),
    txRequiredConfirmations: config.get<number>(
      'txpot.txRequiredConfirmations',
    ),
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
  logs: config.has('logs') ? config.get<Logs[]>('logs') : [],
};
