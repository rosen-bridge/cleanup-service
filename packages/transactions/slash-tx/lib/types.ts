import * as ergoLib from 'ergo-lib-wasm-nodejs';

/**
 * Represents the RWT repository box tracking total RWT and RSN
 */
export interface RWTRepoData {
  box: ergoLib.ErgoBox;
  repoNFT: string; // RepoNFT token ID
  rwtTokenId: string; // RWT token ID
  rsnTokenId: string; // RSN token ID
  awcTokenId: string; // AWC NFT token ID
}
