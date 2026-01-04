/**
 * Workflow configuration.
 */
export interface WorkflowConfig {
    readonly contractsPath: string;
    readonly ergoNetwork: 'mainnet' | 'testnet';
    readonly cleanupMnemonic: string;
    readonly minBoxValue: bigint;
    readonly minCleanupValue: bigint;
    readonly txFee: string;
  }
  
  
  /**
   * TxPot configuration.
   */
  export interface TxPotConfig {
    readonly updateInterval: number; // seconds
    readonly txRequiredConfirmations: number;
  }
  
  
  export interface ScannerConfig {
    readonly updateInterval: number; // seconds
    readonly nodeUrl: string;
    readonly explorerUrl: string;
    readonly initialHeight: number;
  }
  
  /**
   * Log types.
   */
  export type LogType = 'file' | 'console' | 'loki';

  /**
   * Log configuration.
   */
  export interface LogConfig {
    readonly type: LogType;
    readonly level: string;
    readonly maxSize?: string;
    readonly maxFiles?: string;
    readonly path?: string;
    readonly serviceName?: string;
    readonly host?: string;
    readonly basicAuth?: string;
  }
  
  
  /**
   * Database configuration.
   */
  export interface DatabaseConfig {
    readonly type: 'sqlite' | 'postgres';
    readonly path?: string; // sqlite
    readonly host?: string; // postgres
    readonly port?: number; // postgres
    readonly user?: string; // postgres
    readonly password?: string; // postgres
    readonly name?: string; // postgres
  }
  
  
  /**
   * BoxLookup configuration.
   */
  export interface BoxLookupConfig {
    readonly updateInterval: number; // seconds
    readonly nodeUrl: string;
  }
  