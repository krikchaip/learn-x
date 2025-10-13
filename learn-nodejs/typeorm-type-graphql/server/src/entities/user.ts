import { Column, Entity, BeforeInsert, BeforeUpdate, Index } from 'typeorm'
import { Field, InputType, ObjectType } from 'type-graphql'
import { IsNotEmpty, MinLength } from 'class-validator'
import { hashSync, compareSync } from 'bcrypt'

import { CustomEntity } from 'lib/custom_entity'
import { NotExistsIn } from 'lib/validators'

@ObjectType()
@Entity()
@Index(['username'])
export class User extends CustomEntity {
  @Field()
  @Column({ unique: true })
  @NotExistsIn(User)
  @IsNotEmpty({ message: 'Should not be empty!' })
  username: string

  @Column()
  @MinLength(3, { message: 'Minimum length of 3!' })
  password: string

  @BeforeInsert()
  @BeforeUpdate()
  hashPassword() {
    this.password = hashSync(this.password, 10)
  }

  validatePassword(password: string) {
    return compareSync(password, this.password)
  }
}

@InputType()
export class UserCreateInput implements Partial<User> {
  @Field()
  username: string

  @Field()
  password: string
}

@InputType()
export class UserUpdateInput implements Partial<User> {
  @Field({ nullable: true })
  password?: string
}

@InputType()
export class AuthenticateInput implements Partial<User> {
  @Field()
  username: string

  @Field()
  password: string
}

@ObjectType()
export class Authenticate {
  @Field()
  token: string

  @Field(of => User)
  user: User
}
