import './bootstrap';
import { dataSource } from './dataSource';

const main = async () => {
  await dataSource.initialize();
  await dataSource.runMigrations();
};

main();
