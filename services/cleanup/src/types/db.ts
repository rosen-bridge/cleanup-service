export interface DatabaseConfig {
  readonly type: 'sqlite' | 'postgres';
  readonly path?: string; // sqlite
  readonly host?: string; // postgres
  readonly port?: number; // postgres
  readonly user?: string; // postgres
  readonly password?: string; // postgres
  readonly name?: string; // postgres
}


