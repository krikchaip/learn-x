import { DataTypes } from 'sequelize'

import User from '@model/user'
import Post from '@model/post'

User.hasMany(Post, {
  foreignKey: { name: 'creator', allowNull: false },
  keyType: DataTypes.UUID,
  onDelete: 'CASCADE',
  onUpdate: 'CASCADE'
})
Post.belongsTo(User, {
  foreignKey: 'creator'
})
