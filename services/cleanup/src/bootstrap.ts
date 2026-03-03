import '@rosen-bridge/extended-typeorm/bootstrap';

import 'reflect-metadata';

import packageJson from '../package.json' with { type: 'json' };
import { initLogger, getLogger } from './config/loggerConfig';

initLogger();

const logger = getLogger(import.meta.url);

logger.info(`Cleanup service version: ${packageJson.version}`);
