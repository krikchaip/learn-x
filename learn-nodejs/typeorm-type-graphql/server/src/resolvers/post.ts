import { Resolver } from 'type-graphql'
import { Service } from 'typedi'

import { Post, PostCreateInput, PostUpdateInput } from 'entities'
import { createResolvers } from 'lib/crud_resolver'

const { QueryResolver, MutationResolver } = createResolvers({
  Entity: Post,
  EntityCreateType: PostCreateInput,
  EntityUpdateType: PostUpdateInput
})

@Service()
@Resolver()
export class PostQueryResolver extends QueryResolver {}

@Service()
@Resolver()
export class PostMutationResolver extends MutationResolver {}
