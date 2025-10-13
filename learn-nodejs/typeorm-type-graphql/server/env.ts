require('dotenv').config()

/** **[required]** a secret key for `token` service */
export const SECRET = process.env.SECRET

/** **[configurable]** GraphQL server port */
export const SERVER_PORT = process.env.SERVER_PORT || 3000

/** **[configurable]** database port (for TypeORM) */
export const DB_PORT = Number(process.env.DB_PORT) || 5432

export const DEVELOPMENT =
  process.env.NODE_ENV === 'development' || !process.env.NODE_ENV

export const PRODUCTION = process.env.NODE_ENV === 'production'

export const IS_CI = require('is-ci') as boolean

/** determine what `ConfigMode` to use in `ormconfig` */
export const CURRENT =
  DEVELOPMENT || PRODUCTION ? 'default' : process.env.NODE_ENV
