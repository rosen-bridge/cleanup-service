# @rosen-bridge/fraud-tx

Transaction builder for creating fraud boxes from trigger event boxes in the Rosen Bridge cleanup service.

## Overview

This package provides the `FraudTx` class and `FraudTxBuilder` for constructing unsigned Ergo transactions that:

- Consume a trigger event box containing watcher IDs (WIDs) suspected of fraud
- Create individual fraud boxes for each watcher, distributing RWT tokens equally
- Maintain a cleaner box for future cleanup operations

This is the TypeScript implementation of the Scala `generateFrauds` transaction from the original cleanup-service.

## Installation

```bash
npm install @rosen-bridge/fraud-tx
```

## Usage

```typescript
import * as ergoLib from 'ergo-lib-wasm-nodejs';

import { FraudTx, TriggerEventData } from '@rosen-bridge/fraud-tx';


const fraudTx = new FraudTx(
  'fraud_contract_address', // fraudAddress
  'cleaner_address', // cleanerAddress
  'rwt_token_id', // rwtTokenId
  1000000n, // minBoxValue
  '1100000', // txFee
  logger, // optional logger
);

// Prepare trigger event data
const triggerEventData: TriggerEventData = {
  box: ergoLibBox,
  wids: ['wid1hex', 'wid2hex', 'wid3hex'],
  rwtAmount: 1000000n,
};

// Build transaction
const fraudTx = FraudTx.getInstance();
const builder = fraudTx.newBuilder(
  triggerEventData,
  cleanerBox,
  currentHeight,
  [feeBox1, feeBox2], // boxes to cover additional ERG needs
  changeAddress, // optional: defaults to cleanerAddress
);

const fraudTxBuilder = fraudTx.newBuilder()
  .setTriggerEventData(triggerEventData)
  .setCleanerBox(cleanerBox)
  .setCreationHeight(1000)
  .setFeeBoxes(feeBoxes)
  .setChangeAddress(testFraudConfig.fraudAddress);
const { unsignedTx, inputBoxes } = await builder.build();

// Sign and submit transaction
```

## API

### FraudTx

class that manages fraud transaction creation.

#### Methods

- `newBuilder(triggerEventData, cleanerBox, height, feeBoxes, changeAddress?): FraudTxBuilder` - Create a new builder

### FraudTxBuilder

Builder class for constructing fraud transactions.

#### Methods

- `build(): Promise<{ unsignedTx, inputBoxes }>` - Build the unsigned transaction

## Transaction Logic

The fraud transaction:

1. **Inputs**: Trigger event box, cleaner box, optional fee boxes
2. **Outputs**: Multiple fraud boxes (one per WID), new cleaner box
3. **RWT Distribution**: Total RWT from trigger event divided equally among fraud boxes
4. **Registers**: Each fraud box's R4 register contains the watcher's WID as `Coll[Coll[Byte]]`

## License

MIT
