import Sequelize from 'sequelize'

import sequelize from '../utils/sequelize.mjs'

const { DataTypes } = Sequelize

const User = sequelize.define('user', {
  id: {
    type: DataTypes.INTEGER,
    primaryKey: true,
    allowNull: false,
    autoIncrement: true,
  },
  name: DataTypes.STRING,
  email: DataTypes.STRING,
})

export default User
