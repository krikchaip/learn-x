import { Entity, Column } from 'typeorm'
import { Field, InputType, ObjectType } from 'type-graphql'
import { IsNotEmpty } from 'class-validator'

import { CustomEntity } from 'lib/custom_entity'

@ObjectType()
@Entity()
export class Post extends CustomEntity {
  @Field()
  @Column()
  @IsNotEmpty()
  title: string
}

@InputType()
export class PostCreateInput implements Partial<Post> {
  @Field()
  title: string
}

@InputType()
export class PostUpdateInput implements Partial<Post> {
  @Field({ nullable: true })
  title?: string
}
