import { DataTypes, Model } from 'sequelize'

import sequelize from '@config/sequelize'

interface PostAttributes {
  id: UUID
  title: STRING
  content?: TEXT
  imageURL?: TEXT
}

interface PostCreationAttributes extends Omit<PostAttributes, 'id'> {}

export interface PostInstance
  extends Model<PostAttributes, PostCreationAttributes>,
    PostAttributes {}

const Post = sequelize.define<PostInstance>('post', {
  id: {
    type: DataTypes.UUID,
    defaultValue: DataTypes.UUIDV4,
    primaryKey: true
  },
  title: { type: DataTypes.STRING, allowNull: false },
  content: DataTypes.TEXT,
  imageURL: DataTypes.TEXT
})

export default Post
