import { ConfigValidator } from '@rosen-bridge/config';
import config from 'config';
import * as fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

import type { CleanupServiceConfig, Workflow } from '../types';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export type CleanupServiceConfigResolved = Omit<
  CleanupServiceConfig,
  'workflow'
> & {
  workflow: Omit<Workflow, 'minBoxValue' | 'minCleanupValue'> & {
    minBoxValue: bigint;
    minCleanupValue: bigint;
  };
};

/**
 * Validates config using the config schema and returns config with workflow bigint fields resolved.
 */
export const validateConfigs = (): CleanupServiceConfigResolved => {
  const rawSchemaData = fs.readFileSync(
    path.join(__dirname, '../../config/schema.json'),
    'utf-8',
  );
  const schema = JSON.parse(rawSchemaData);
  const confValidator = new ConfigValidator(schema);
  const raw = config.util.toObject() as CleanupServiceConfig;
  confValidator.validateConfig(raw);

  const workflow = {
    ...raw.workflow,
    minBoxValue: BigInt(raw.workflow.minBoxValue),
    minCleanupValue: BigInt(raw.workflow.minCleanupValue),
  };

  return { ...raw, workflow };
};
