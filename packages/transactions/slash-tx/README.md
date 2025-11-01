# @rosen-bridge/slash-tx

Transaction builder for slashing fraudulent watchers in the Rosen Bridge cleanup service.

## Overview

This package provides the `SlashTx` class and `SlashTxBuilder` for constructing unsigned Ergo transactions that:

- Consume a fraud box containing a watcher's WID and RWT tokens
- Consume a collateral box containing the watcher's RSN collateral
- Update the RWT repository box (increase total RWT, decrease total RSN)
- Update the collateral box (decrease RSN amount in R5 register)
- Maintain a cleanup box for future cleanup operations

This is the TypeScript implementation based on the reference `slashRSN` implementation.

## Installation

```bash
npm install @rosen-bridge/slash-tx
```

## Usage

```typescript
import {
  SlashTx,
  FraudBoxData,
  CollateralBoxData,
  RWTRepoData,
} from '@rosen-bridge/slash-tx';
import * as ergoLib from 'ergo-lib-wasm-nodejs';

// Initialize SlashTx singleton
SlashTx.init(
  'repo_contract_address', // repoAddress
  'collateral_contract_address', // collateralAddress
  'cleanup_address', // cleanupAddress
  1000000n, // minBoxValue
  '1100000', // txFee
  logger, // optional logger
);

// Prepare fraud box data
const fraudBoxData: FraudBoxData = {
  box: fraudErgoBox,
  wid: 'watcher_id_hex',
  rwtAmount: 1000000n,
};

// Prepare collateral box data
const collateralBoxData: CollateralBoxData = {
  box: collateralErgoBox,
  wid: 'watcher_id_hex', // Must match fraud box WID
  rsnAmount: 800000n, // From R5 register
};

// Prepare repo data
const repoData: RWTRepoData = {
  box: repoErgoBox,
  repoNFT: 'repo_nft_token_id',
  rwtTokenId: 'rwt_token_id',
  rsnTokenId: 'rsn_token_id',
};

// Build transaction
const slashTx = SlashTx.getInstance();
const builder = slashTx.newBuilder(
  fraudBoxData,
  collateralBoxData,
  repoData,
  cleanupBox,
  currentHeight,
  [feeBox1, feeBox2], // boxes to cover additional ERG needs
  changeAddress, // optional: defaults to cleanupAddress
);

const { unsignedTx, inputBoxes } = await builder.build();

// Sign and submit transaction
```

## API

### SlashTx

Singleton class that manages slash transaction creation.

#### Methods

- `static init(repoAddress, collateralAddress, cleanupAddress, minBoxValue, txFee, logger?): void` - Initialize the singleton
- `static getInstance(): SlashTx` - Get the singleton instance
- `newBuilder(fraudBoxData, collateralBoxData, repoData, cleanupBox, height, feeBoxes, changeAddress?): SlashTxBuilder` - Create a new builder

### SlashTxBuilder

Builder class for constructing slash transactions.

#### Methods

- `build(): Promise<{ unsignedTx, inputBoxes }>` - Build the unsigned transaction

## Transaction Logic

The slash transaction:

1. **Inputs**: RWT repo box, collateral box, fraud box, cleanup box, optional fee boxes
2. **Outputs**: Updated repo box, updated collateral box, new cleanup box, change box (if needed)
3. **Repo Update**: Increases total RWT by slashed amount, RSN stays unchanged
4. **Collateral Update**: Decreases RSN token and R5 register by slashed amount
5. **Validation**:
   - WID in fraud box must match WID in collateral box
   - Collateral RSN amount must be >= slashed RWT amount
   - All boxes must have valid values and tokens

## License

GPL-3.0
