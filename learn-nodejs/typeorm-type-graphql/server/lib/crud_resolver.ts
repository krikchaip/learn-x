// TypeORM Entity#findOne returns unexpected value
// ref: https://stackoverflow.com/questions/53455552

import {
  Arg,
  ClassType,
  FieldResolver,
  ID,
  Mutation,
  ObjectType,
  Query,
  Resolver,
  Root
} from 'type-graphql'
import { BaseEntity } from 'typeorm'

export function createResolvers<T extends typeof BaseEntity>(options: {
  Entity: T
  EntityCreateType?: ClassType<Partial<InstanceType<T>>>
  EntityUpdateType: ClassType<Partial<InstanceType<T>>>
}) {
  const { Entity, EntityCreateType = Entity, EntityUpdateType } = options

  @ObjectType(`${Entity.name}Query`)
  class EntityQuery {}

  @Resolver(of => EntityQuery, { isAbstract: true })
  abstract class QueryResolver {
    @Query(of => EntityQuery)
    [Entity.name.toLowerCase()](
      @Arg('id', of => ID, { nullable: true }) id?: string
    ) {
      return { id }
    }

    @FieldResolver(of => Entity, { nullable: true })
    async info(@Root('id') id?: string) {
      return (await Entity.findOne({ where: { id } })) || null
    }

    @FieldResolver(of => [Entity])
    async list(@Root('id') id?: string) {
      if (!id) return Entity.find()
      const item = await Entity.findOne({ where: { id } })
      return item ? [item] : []
    }
  }

  @ObjectType(`${Entity.name}Mutation`)
  class EntityMutation {}

  @Resolver(of => EntityMutation, { isAbstract: true })
  abstract class MutationResolver {
    @Mutation(of => EntityMutation)
    [Entity.name.toLowerCase()](
      @Arg('id', of => ID, { nullable: true }) id?: string
    ) {
      return { id }
    }

    @FieldResolver(of => Entity, { nullable: true })
    create(@Arg('data', of => EntityCreateType) data: InstanceType<T>) {
      return Entity.create(data).save()
    }

    @FieldResolver(of => Entity, { nullable: true })
    async update(
      @Arg('data', of => EntityUpdateType) data: InstanceType<T>,
      @Root('id') id?: string
    ) {
      const item = await Entity.findOne({ where: { id } })
      return item ? Object.assign(item, data).save() : null
    }

    @FieldResolver(of => Entity, { nullable: true })
    async delete(@Root('id') id?: string) {
      const item = await Entity.findOne({ where: { id } })
      return item ? item.remove() : null
    }
  }

  return {
    QueryResolver,
    MutationResolver,
    EntityQuery,
    EntityMutation
  }
}
