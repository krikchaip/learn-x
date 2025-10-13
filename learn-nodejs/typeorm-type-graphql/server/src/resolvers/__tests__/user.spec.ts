import { buildSchemaSync } from 'type-graphql'
import { Container } from 'typedi'
import { ApolloServer } from 'apollo-server'
import { Connection, createConnection } from 'typeorm'

import { UserCreateInput } from 'entities'
import { UserQueryResolver, UserMutationResolver } from '../user'
import { current } from 'ormconfig'

jest.mock('services/token')

beforeAll(async () => {
  const schema = buildSchemaSync({
    resolvers: [UserQueryResolver, UserMutationResolver],
    container: Container,
    validate: false // https://github.com/MichalLytek/type-graphql/issues/150
  })

  db = await createConnection(current)
  server = new ApolloServer({ schema })

  // creating a mock user
  await server.executeOperation({
    query: query.create,
    variables: input.create
  })
})

afterAll(async () => {
  return db.close()
})

describe('Mutation', () => {
  describe('authenticate', () => {
    it('"null" when no user found', async () => {
      const result = await server.executeOperation({
        query: query.authenticate,
        variables: {
          username: 'DUMB_USER',
          password: '11111'
        }
      })

      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "user": Object {
            "authenticate": null,
          },
        }
      `)
    })

    it('"null" when enter a wrong password', async () => {
      const result = await server.executeOperation({
        query: query.authenticate,
        variables: {
          username: input.create.username,
          password: 'WRONG_PASSWORD'
        }
      })

      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "user": Object {
            "authenticate": null,
          },
        }
      `)
    })

    it('"{ token, data }" when everything is right', async () => {
      const result = await server.executeOperation({
        query: query.authenticate,
        variables: input.create
      })

      expect(result.data!.user).toEqual({
        authenticate: {
          token: expect.any(String),
          user: { id: expect.any(String) }
        }
      })
    })
  })
})

let db: Connection, server: ApolloServer

const input = {
  create: {
    username: 'winner',
    password: 'zaza555'
  } as UserCreateInput
}

const query = {
  create: `#graphql
    mutation ($username: String!, $password: String!) {
      user {
        create(data: { username: $username, password: $password }) {
          id
        }
      }
    }
  `,
  authenticate: `#graphql
    mutation ($username: String!, $password: String!) {
      user {
        authenticate(data: { username: $username, password: $password }) {
          token
          user {
            id
          }
        }
      }
    }
  `
}
