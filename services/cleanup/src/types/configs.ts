export interface CleanupServiceConfig {
  boxLookup: BoxLookup;
  scanner: Scanner;
  txpot: Txpot;
  database: Database;
  logs: Logs[];
  workflow: Workflow;
  intervals: Intervals;
}

export interface Intervals {
  workflow: number;
}

export interface Workflow {
  contractsPath: string;
  ergoNetwork: 'mainnet' | 'testnet';
  cleanupMnemonic: string;
  minBoxValue: bigint;
  minCleanupValue: bigint;
  txFee: string;
}

export interface Logs {
  type: 'file' | 'console' | 'loki';
  level: string;
  path?: string;
  maxSize?: string;
  maxFiles?: string;
  serviceName?: string;
  host?: string;
  basicAuth?: string;
}

export interface Database {
  type: 'sqlite' | 'postgres';
  path?: string;
  host?: string;
  port?: number;
  user?: string;
  password?: string;
  name?: string;
}

export interface Txpot {
  updateInterval: number;
  txRequiredConfirmations: number;
}

export interface Scanner {
  updateInterval: number;
  nodeUrl: string;
  explorerUrl: string;
  initialHeight: number;
}

export interface BoxLookup {
  updateInterval: number;
  nodeUrl: string;
}
