import {
  BlockEntity,
  ExtractorStatusEntity,
} from '@rosen-bridge/abstract-scanner';
import { BoxEntity } from '@rosen-bridge/address-extractor';
import { FraudEntity } from '@rosen-bridge/fraud-extractor';
import { TransactionEntity } from '@rosen-bridge/tx-pot';
import {
  CollateralEntity,
  CommitmentEntity,
  EventTriggerEntity,
} from '@rosen-bridge/watcher-data-extractor';

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
