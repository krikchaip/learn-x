import {
  Association,
  DataTypes,
  HasManyCreateAssociationMixin,
  Model,
  ModelCtor
} from 'sequelize'
import bcrypt from 'bcrypt'

import sequelize from '@config/sequelize'
import { PostInstance } from '@model/post'

interface UserAttributes {
  id: UUID
  email: STRING
  password: STRING
  role: Role
  name?: STRING
  status?: TEXT
}

interface UserCreationAttributes extends Omit<UserAttributes, 'id'> {}

interface UserInstance
  extends Model<UserAttributes, UserCreationAttributes>,
    UserAttributes {
  createPost: HasManyCreateAssociationMixin<PostInstance>
}

interface UserCtor extends ModelCtor<UserInstance> {
  associations: {
    posts: Association<UserInstance, PostInstance>
  }
}

export enum Role {
  USER = 'USER',
  ADMIN = 'ADMIN'
}

const User = <UserCtor>sequelize.define<UserInstance>('user', {
  id: {
    type: DataTypes.UUID,
    defaultValue: DataTypes.UUIDV4,
    primaryKey: true
  },
  email: {
    type: DataTypes.STRING,
    allowNull: false,
    unique: true,
    validate: { isEmail: true }
  },
  password: {
    type: DataTypes.STRING,
    allowNull: false,
    set(val: string) {
      this.setDataValue('password', bcrypt.hashSync(val, 10))
    }
  },
  role: {
    type: DataTypes.ENUM,
    values: [Role.USER, Role.ADMIN],
    allowNull: false
  },
  name: DataTypes.STRING,
  status: DataTypes.TEXT
})

export const matchPassword = async (user: UserInstance, password: string) => {
  return bcrypt.compare(password, user.password)
}

export default User
