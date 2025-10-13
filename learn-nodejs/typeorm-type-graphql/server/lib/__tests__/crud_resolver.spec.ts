import {
  buildSchemaSync,
  getMetadataStorage,
  Field,
  InputType,
  ObjectType,
  Resolver
} from 'type-graphql'
import { BaseEntity } from 'typeorm'
import { ApolloServer } from 'apollo-server'
import { mocked } from 'ts-jest/utils'

import { createResolvers } from 'lib/crud_resolver'

jest.mock('typeorm/repository/BaseEntity')

beforeEach(() => {
  // Clearing TypeGraphQL's decorator metadata between tests.
  // Without this, we can't call "setup" more than once.
  getMetadataStorage().clear()
})

describe('QueryResolver', () => {
  describe('entity#info', () => {
    it('entity(id: "...").info -> data | null', async () => {
      const { server, QueryResolver, Entity } = setup()

      const id = 'TEST_ID'
      const data = { field: 'FIELD_VALUE' } as any

      const info = jest.spyOn(QueryResolver.prototype, 'info')
      Entity.findOne.mockImplementationOnce(async () => data)

      let result

      result = await server.executeOperation({
        query: `#graphql
          query ($id: ID!) {
            entity(id: $id) {
              info {
                field
              }
            }
          }
        `,
        variables: { id }
      })

      expect(info).toBeCalledWith(id)
      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "entity": Object {
            "info": Object {
              "field": "FIELD_VALUE",
            },
          },
        }
      `)

      info.mockClear()
      Entity.findOne.mockImplementationOnce(async () => undefined)

      result = await server.executeOperation({
        query: `#graphql
          query ($id: ID!) {
            entity(id: $id) {
              info {
                field
              }
            }
          }
        `,
        variables: { id }
      })

      expect(info).toBeCalledWith(id)
      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "entity": Object {
            "info": null,
          },
        }
      `)
    })

    it('entity.info -> null', async () => {
      const { server, QueryResolver, Entity } = setup()

      const info = jest.spyOn(QueryResolver.prototype, 'info')
      Entity.findOne.mockImplementationOnce(async () => undefined)

      const result = await server.executeOperation({
        query: `#graphql
          query {
            entity {
              info {
                field
              }
            }
          }
        `
      })

      expect(info).toBeCalledWith(undefined)
      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "entity": Object {
            "info": null,
          },
        }
      `)
    })
  })

  describe('entity#list', () => {
    it('entity(id: "...").list -> [data] | []', async () => {
      const { server, QueryResolver, Entity } = setup()

      const id = 'TEST_ID'
      const data = { field: 'FIELD_VALUE' } as any

      const list = jest.spyOn(QueryResolver.prototype, 'list')
      Entity.findOne.mockImplementationOnce(async () => data)

      let result

      result = await server.executeOperation({
        query: `#graphql
          query ($id: ID!) {
            entity(id: $id) {
              list {
                field
              }
            }
          }
        `,
        variables: { id }
      })

      expect(list).toBeCalledWith(id)
      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "entity": Object {
            "list": Array [
              Object {
                "field": "FIELD_VALUE",
              },
            ],
          },
        }
      `)

      list.mockClear()
      Entity.findOne.mockImplementationOnce(async () => undefined)

      result = await server.executeOperation({
        query: `#graphql
          query ($id: ID!) {
            entity(id: $id) {
              list {
                field
              }
            }
          }
        `,
        variables: { id }
      })

      expect(list).toBeCalledWith(id)
      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "entity": Object {
            "list": Array [],
          },
        }
      `)
    })

    it('entity.list -> [data]', async () => {
      const { server, QueryResolver, Entity } = setup()

      const data = [
        { field: 'FIELD_VALUE_1' },
        { field: 'FIELD_VALUE_2' }
      ] as any

      const list = jest.spyOn(QueryResolver.prototype, 'list')
      Entity.find.mockImplementationOnce(async () => data)

      const result = await server.executeOperation({
        query: `#graphql
          query {
            entity {
              list {
                field
              }
            }
          }
        `
      })

      expect(list).toBeCalledWith(undefined)
      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "entity": Object {
            "list": Array [
              Object {
                "field": "FIELD_VALUE_1",
              },
              Object {
                "field": "FIELD_VALUE_2",
              },
            ],
          },
        }
      `)
    })
  })
})

describe('MutationResolver', () => {
  describe('entity#create', () => {
    let $Entity: typeof BaseEntity

    beforeEach(() => {
      @ObjectType('Entity')
      @InputType('EntityCreateType')
      class Entity extends BaseEntity {
        @Field()
        field1: string

        @Field()
        field2: string
      }

      $Entity = Entity
    })

    it('use input type from Entity definition', async () => {
      const { server, MutationResolver } = setup({ Entity: $Entity })

      const data = {
        field1: 'TEST_CREATE_FIELD_1',
        field2: 'TEST_CREATE_FIELD_2'
      } as any

      const create = jest.spyOn(MutationResolver.prototype, 'create')
      create.mockReturnValueOnce(data)

      const result = await server.executeOperation({
        query: `#graphql
          mutation ($data: EntityCreateType!) {
            entity {
              create(data: $data) {
                field1
                field2
              }
            }
          }
        `,
        variables: { data }
      })

      expect(create).toBeCalledWith(expect.objectContaining(data))
      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "entity": Object {
            "create": Object {
              "field1": "TEST_CREATE_FIELD_1",
              "field2": "TEST_CREATE_FIELD_2",
            },
          },
        }
      `)
    })

    it('providing EntityCreateType', async () => {
      @InputType()
      class EntityCreateType {
        @Field()
        field1: string

        @Field({ nullable: true })
        field2?: string
      }

      const { server, MutationResolver } = setup({
        Entity: $Entity,
        EntityCreateType: EntityCreateType as any
      })

      const data = { field1: 'TEST_PROVIDING_CREATE_TYPE' } as any

      const create = jest.spyOn(MutationResolver.prototype, 'create')
      create.mockReturnValueOnce(data)

      const result = await server.executeOperation({
        query: `#graphql
          mutation ($data: EntityCreateType!) {
            entity {
              create(data: $data) {
                field1
                field2
              }
            }
          }
        `,
        variables: { data }
      })

      expect(create).toBeCalledWith(expect.objectContaining(data))
      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "entity": Object {
            "create": Object {
              "field1": "TEST_PROVIDING_CREATE_TYPE",
              "field2": "Hello World",
            },
          },
        }
      `)
    })
  })

  describe('entity#update', () => {
    it('return updated data', async () => {
      const { server, MutationResolver, Entity } = setup()

      const id = 'FOUND!'
      const data = { field: 'AFTER_UPDATE' } as any
      const item = { field: 'BEFORE_UPDATE', save: jest.fn(() => data) } as any

      const update = jest.spyOn(MutationResolver.prototype, 'update')
      Entity.findOne.mockResolvedValueOnce(item)

      const result = await server.executeOperation({
        query: `#graphql
          mutation ($id: ID!, $data: EntityUpdateType!) {
            entity(id: $id) {
              update(data: $data) {
                field
              }
            }
          }
        `,
        variables: { data, id }
      })

      expect(update).toBeCalledWith(data, id)
      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "entity": Object {
            "update": Object {
              "field": "AFTER_UPDATE",
            },
          },
        }
      `)
    })

    it('return null when not found', async () => {
      const { server, MutationResolver, Entity } = setup()

      const id = 'THIS_SHOULD_NOT_BE_FOUND'
      const data = { field: 'TEST_UPDATE' } as any

      const update = jest.spyOn(MutationResolver.prototype, 'update')
      Entity.findOne.mockResolvedValueOnce(undefined)

      const result = await server.executeOperation({
        query: `#graphql
          mutation ($id: ID!, $data: EntityUpdateType!) {
            entity(id: $id) {
              update(data: $data) {
                field
              }
            }
          }
        `,
        variables: { data, id }
      })

      expect(update).toBeCalledWith(data, id)
      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "entity": Object {
            "update": null,
          },
        }
      `)
    })
  })

  describe('entity#delete', () => {
    it('return deleted data', async () => {
      const { server, MutationResolver, Entity } = setup()

      const id = 'FOUND!'
      const data = { field: 'DELETED_DATA' } as any

      const remove = jest.spyOn(MutationResolver.prototype, 'delete')
      Entity.findOne.mockResolvedValueOnce({ ...data, remove: () => data })

      const result = await server.executeOperation({
        query: `#graphql
          mutation ($id: ID!) {
            entity(id: $id) {
              delete {
                field
              }
            }
          }
        `,
        variables: { id }
      })

      expect(remove).toBeCalledWith(id)
      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "entity": Object {
            "delete": Object {
              "field": "DELETED_DATA",
            },
          },
        }
      `)
    })

    it('return null when not found', async () => {
      const { server, MutationResolver, Entity } = setup()

      const id = 'THIS_SHOULD_NOT_BE_FOUND'

      const remove = jest.spyOn(MutationResolver.prototype, 'delete')
      Entity.findOne.mockResolvedValueOnce(undefined)

      const result = await server.executeOperation({
        query: `#graphql
          mutation ($id: ID!) {
            entity(id: $id) {
              delete {
                field
              }
            }
          }
        `,
        variables: { id }
      })

      expect(remove).toBeCalledWith(id)
      expect(result.data).toMatchInlineSnapshot(`
        Object {
          "entity": Object {
            "delete": null,
          },
        }
      `)
    })
  })
})

function setup(options?: Partial<Parameters<typeof createResolvers>[0]>) {
  // You can only annotate each TypeGraphQL's type decorator
  // (ObjectType, InputType, ...) once for each ES6 class.
  // Doing more than that would overide all the previous one.
  @ObjectType('Entity')
  @InputType('EntityCreateType')
  // @InputType('EntityUpdate') ❌ this would override "EntityCreate"
  class Entity extends BaseEntity {
    @Field()
    field: string
  }

  @InputType()
  class EntityUpdateType implements Partial<Entity> {
    @Field({ nullable: true })
    field?: string
  }

  const { QueryResolver, MutationResolver } = createResolvers({
    Entity,
    EntityUpdateType,
    ...options
  })

  @Resolver()
  class EntityQueryResolver extends QueryResolver {}

  @Resolver()
  class EntityMutationResolver extends MutationResolver {}

  const schema = buildSchemaSync({
    resolvers: [EntityQueryResolver, EntityMutationResolver],
    validate: false // https://github.com/MichalLytek/type-graphql/issues/150
  })

  const server = new ApolloServer({
    schema,
    mocks: true,
    mockEntireSchema: false
  })

  return {
    Entity: mocked(Entity),
    QueryResolver: EntityQueryResolver,
    MutationResolver: EntityMutationResolver,
    server
  }
}
