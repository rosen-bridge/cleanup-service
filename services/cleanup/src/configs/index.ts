import config from 'config';

export const maxLogSize = config.get<string>('logs.maxSize');
export const maxLogFilesCount = config.get<string>('logs.maxFilesCount');
export const logsPath = config.get<string>('logs.path');
export const logLevel = config.get<string>('logs.level');
