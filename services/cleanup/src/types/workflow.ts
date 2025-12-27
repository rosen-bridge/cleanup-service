export interface WorkflowConfig {
  readonly contractsPath: string;
  readonly ergoNetwork: 'mainnet' | 'testnet';
  readonly cleanupMnemonic: string;
  readonly minBoxValue: bigint;
  readonly txFee: string;
}


