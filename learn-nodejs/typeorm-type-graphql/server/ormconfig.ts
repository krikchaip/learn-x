import { ConnectionOptions, DefaultNamingStrategy } from 'typeorm'
import { omit } from 'ramda'

import { CURRENT, IS_CI, DB_PORT } from 'env'

class NamingStrategy extends DefaultNamingStrategy {
  joinColumnName = (prop: string) => prop + '_id'
  joinTableColumnName = (table: string) => table + '_id'
  joinTableName = (table1: string, table2: string) => table1 + ' & ' + table2
}

type ConfigMode = 'default' | 'test'
export const ormconfig: Record<ConfigMode, ConnectionOptions> = {
  default: {
    name: 'default',
    type: 'postgres',
    host: 'localhost',
    port: DB_PORT,
    username: 'postgres',
    password: 'postgres',
    database: 'postgres',
    schema: 'public',
    synchronize: false,
    migrationsRun: false,
    logging: false,
    entities: ['src/entities/!(index).{js,ts}'],
    migrationsTableName: '__migration__',
    cli: { migrationsDir: 'db/migration' },
    namingStrategy: new NamingStrategy()
  },
  test: {
    name: 'test',
    type: 'postgres',
    host: 'localhost',
    port: DB_PORT,
    username: 'postgres',
    password: 'postgres',
    database: 'postgres',
    schema: IS_CI ? 'public' : 'test',
    dropSchema: true,
    synchronize: true,
    logging: false,
    entities: ['src/entities/!(index).ts'],
    namingStrategy: new NamingStrategy()
  }
}

// "createConnection" parameter can't have the "name" property
// when passing as an object.
export const current = omit(
  ['name'],
  ormconfig[CURRENT as ConfigMode]
) as ConnectionOptions

export default Object.values(ormconfig) as ConnectionOptions[]
