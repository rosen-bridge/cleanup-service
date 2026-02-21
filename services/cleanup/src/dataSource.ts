import { DataSource } from '@rosen-bridge/extended-typeorm';

import { configs } from './configs/config';
import entities from './db/entities';
import migrations from './db/migrations';

const db = configs.database;

const common = {
  entities,
  migrations: migrations[db.type],
  synchronize: false,
  logging: false,
} as const;

let dataSource: DataSource;
if (db.type === 'sqlite') {
  dataSource = new DataSource({
    type: 'sqlite',
    database: db.path ?? ':memory:',
    ...common,
  });
} else if (db.type === 'postgres') {
  dataSource = new DataSource({
    type: 'postgres',
    host: db.host,
    port: db.port,
    username: db.user,
    password: db.password,
    database: db.name,
    ...common,
  });
} else {
  throw new Error(
    `Database type=[${(db as { type: string }).type}] not supported`,
  );
}

export default dataSource;
