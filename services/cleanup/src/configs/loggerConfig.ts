import CallbackLogger from '@rosen-bridge/callback-logger';
import WinstonLogger, {
  type TransportOptions,
} from '@rosen-bridge/winston-logger';

import { configs } from './config';
import { Logs } from '../types';

let rootLogger: CallbackLogger | undefined;

export const getLogOptions = (logConfigs: Logs[] = []): TransportOptions[] => {
  const logOptions: TransportOptions[] = [];
  for (const log of logConfigs) {
    switch (log.type) {
      case 'console':
        logOptions.push({
          type: log.type,
          level: log.level,
        });
        break;
      case 'file':
        logOptions.push({
          type: log.type,
          level: log.level,
          path: log.path!,
          maxSize: log.maxSize!,
          maxFiles: log.maxFiles!,
        });
        break;
      case 'loki':
        logOptions.push({
          type: log.type,
          level: log.level,
          serviceName: log.serviceName,
          host: log.host!,
          basicAuth: log.basicAuth,
        });
        break;
    }
  }
  return logOptions;
};

export const initLogger = (): void => {
  if (rootLogger) return; // Already initialized
  const winstonLogger = WinstonLogger.createLogger(getLogOptions(configs.logs));
  rootLogger = new CallbackLogger(winstonLogger);
};

export const getLogger = (path: string): CallbackLogger => {
  if (!rootLogger) {
    initLogger();
  }
  return rootLogger!.child(path);
};
