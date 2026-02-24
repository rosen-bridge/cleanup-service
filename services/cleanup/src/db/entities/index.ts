import { BlockEntity, ExtractorStatusEntity } from '@rosen-bridge/abstract-scanner';
import { TransactionEntity } from '@rosen-bridge/tx-pot';
import { BoxEntity } from '@rosen-bridge/address-extractor';
import {
  CollateralEntity,
  CommitmentEntity,
  EventTriggerEntity,
} from '@rosen-bridge/watcher-data-extractor';
import { FraudEntity } from '@rosen-bridge/fraud-extractor';

/**
 * TypeORM entities required by cleanup-service.
 */
export default [
  TransactionEntity,
  BlockEntity,
  ExtractorStatusEntity,
  BoxEntity,
  EventTriggerEntity,
  FraudEntity,
  CollateralEntity,
  CommitmentEntity,
];


