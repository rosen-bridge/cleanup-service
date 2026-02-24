import { CallbackLoggerFactory } from '@rosen-bridge/callback-logger';
import WinstonLogger from '@rosen-bridge/winston-logger';
import 'reflect-metadata';

import { maxLogSize, maxLogFilesCount, logsPath, logLevel } from './configs';

CallbackLoggerFactory.init(
  new WinstonLogger([
    {
      type: 'file',
      path: logsPath,
      maxSize: maxLogSize,
      maxFiles: maxLogFilesCount,
      level: logLevel,
    },
  ]),
);
