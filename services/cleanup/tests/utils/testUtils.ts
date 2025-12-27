import { DataSource } from '@rosen-bridge/extended-typeorm';
import { DBService } from '../../src/services/dbService';
import { ScannerService } from '../../src/services/scannerService';
import { resetServiceInstance } from '../testUtils';
import { mockNodeUrl } from '../testData';
import { workflowContracts } from '../testData';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { CollateralEntity, EventTriggerEntity } from '@rosen-bridge/watcher-data-extractor';
import { TransactionEntity } from '@rosen-bridge/tx-pot';
import { BoxEntity } from '@rosen-bridge/address-extractor';
import { FraudEntity } from '@rosen-bridge/fraud-extractor';
import { CommitmentEntity } from '@rosen-bridge/watcher-data-extractor';

export const createMemoryDataSource = () =>
    new DataSource({
      type: 'sqlite',
      database: ':memory:',
      entities: [
        TransactionEntity,
        BoxEntity,
        EventTriggerEntity,
        FraudEntity,
        CollateralEntity,
        CommitmentEntity,
      ],
      migrations: [],
      synchronize: true,
      logging: false,
    });
  
  export const initServices = async (updateInterval: number) => {
    const ds = createMemoryDataSource();
    resetServiceInstance(DBService);
    resetServiceInstance(ScannerService);
    DBService.init(ds);
    await DBService.getInstance().startService();
    ScannerService.init(
      updateInterval,
      mockNodeUrl,
      'http://explorer',
      0,
      ergoLib.NetworkPrefix.Mainnet,
      workflowContracts,
      [],
      undefined,
    );
    return ds;
  };

export const dummyUnsignedTx = (outputs: ergoLib.ErgoBoxCandidate[]): ergoLib.UnsignedTransaction => {
  const inputs = new ergoLib.UnsignedInputs();
  inputs.add(
    ergoLib.UnsignedInput.from_box_id(
      ergoLib.BoxId.from_str('0000000000000000000000000000000000000000000000000000000000000000'),
    ),
  );
  const candidates = ergoLib.ErgoBoxCandidates.empty();
  outputs.forEach((b) => candidates.add(b));
  return new ergoLib.UnsignedTransaction(inputs, new ergoLib.DataInputs(), candidates);
};

export const mkCandidate = (address: string, tokenId?: string): ergoLib.ErgoBoxCandidate => {
  const builder = new ergoLib.ErgoBoxCandidateBuilder(
    ergoLib.BoxValue.from_i64(ergoLib.I64.from_str('1000000')),
    ergoLib.Contract.new(ergoLib.Address.from_base58(address).to_ergo_tree()),
    0,
  );
  if (tokenId) {
    builder.add_token(
      ergoLib.TokenId.from_str(tokenId),
      ergoLib.TokenAmount.from_i64(ergoLib.I64.from_str('1')),
    );
  }
  return builder.build();
};