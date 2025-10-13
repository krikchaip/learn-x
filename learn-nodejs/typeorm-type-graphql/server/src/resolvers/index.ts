import path from 'path'
import { buildSchemaSync } from 'type-graphql'
import { Container } from 'typedi'

export const schema = buildSchemaSync({
  resolvers: [path.join(__dirname, '!(index).{js,ts}')],
  container: Container
})
