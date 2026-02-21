import 'reflect-metadata';
import '@rosen-bridge/extended-typeorm/bootstrap';

import packageJson from '../package.json' with { type: 'json' };
import { initLogger, getLogger } from './configs/loggerConfig';

initLogger();

const logger = getLogger(import.meta.url);

logger.info(`Cleanup service version: ${packageJson.version}`);
