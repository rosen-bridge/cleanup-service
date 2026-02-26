/**
 * Rosen contracts.
 */
export interface RosenContracts {
  readonly tokens: {
    readonly RWTId: string;
    readonly RSN: string;
    readonly RepoNFT: string;
    readonly RepoConfigNFT: string;
    readonly CleanupNFT: string;
    readonly AwcNFT: string;
  };
  readonly addresses: {
    readonly RWTRepo: string;
    readonly RepoConfig: string;
    readonly WatcherCollateral: string;
    readonly WatcherPermit: string;
    readonly Commitment: string;
    readonly WatcherTriggerEvent: string;
    readonly Fraud: string;
    readonly lock: string;
    readonly cold: string;
    readonly guardSign: string;
  };
  readonly cleanupConfirm: number;
}
