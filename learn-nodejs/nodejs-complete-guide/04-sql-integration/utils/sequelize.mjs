import Sequelize from 'sequelize'

const sequelize = new Sequelize.Sequelize(
  'nodejs-complete-guide',
  'root',
  'root',
  {
    dialect: 'mysql',
    host: 'localhost',
    port: 3306,
    define: {
      freezeTableName: true,
      timestamps: false,
    },
  }
)

export default sequelize
