import { migrations as scannerMigrations } from '@rosen-bridge/abstract-scanner';
import { migrations as addressExtractorMigrations } from '@rosen-bridge/address-extractor';
import { migrations as fraudExtractorMigrations } from '@rosen-bridge/fraud-extractor';
import { migrations as txPotMigrations } from '@rosen-bridge/tx-pot';
import { migrations as watcherDataExtractorMigrations } from '@rosen-bridge/watcher-data-extractor';

const migrations = {
  sqlite: [
    ...scannerMigrations.sqlite,
    ...txPotMigrations.sqlite,
    ...addressExtractorMigrations.sqlite,
    ...watcherDataExtractorMigrations.sqlite,
    ...fraudExtractorMigrations.sqlite,
  ],
  postgres: [
    ...scannerMigrations.postgres,
    ...txPotMigrations.postgres,
    ...addressExtractorMigrations.postgres,
    ...watcherDataExtractorMigrations.postgres,
    ...fraudExtractorMigrations.postgres,
  ],
};

export default migrations;
