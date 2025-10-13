import { ApolloServer } from 'apollo-server'
import { createConnection } from 'typeorm'

import { SERVER_PORT } from 'env'
import { current } from 'ormconfig'
import { schema } from 'resolvers'

export async function bootstrap() {
  const server = new ApolloServer({
    schema,
    introspection: true
  })

  const db = await createConnection(current)
  const info = await server.listen({ port: SERVER_PORT })

  // eslint-disable-next-line no-console
  console.log(`🚀 Server ready at ${info.url}`)

  return { server: info.server, db }
}
