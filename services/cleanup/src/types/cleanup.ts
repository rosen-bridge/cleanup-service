/**
 * Token ids required to interpret cleanup-related boxes.
 */
export interface CleanupTokenIds {
  rwtTokenId: string;
  rsnTokenId: string;
  repoNftTokenId: string;
  awcTokenId: string;
}

/**
 * Cleanup transaction types.
 */
export enum CleanupTxType {
  fraud = 'fraud',
  slash = 'slash',
}
