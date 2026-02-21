import fs from 'fs';

import { RosenContracts } from '../types';

/**
 * Loads a contract config JSON file.
 *
 * @param filePath - Path to `contracts-<network>-<type>-<version>.json`
 * @returns Parsed contract config
 */
export const loadRosenContracts = (filePath: string): RosenContracts => {
  if (!fs.existsSync(filePath)) {
    throw new Error(
      `contracts config file with path ${filePath} doesn't exist`,
    );
  }
  const raw = fs.readFileSync(filePath, 'utf8');
  return JSON.parse(raw) as RosenContracts;
};
