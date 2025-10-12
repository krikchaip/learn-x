import { Sequelize } from 'sequelize'

import color from '@lib/console-color'

// TODO custom config options based on NODE_ENV
const sequelize = new Sequelize({
  host: 'db',
  port: 5432,
  username: 'postgres',
  password: 'postgres',
  database: '10-graphql',
  dialect: 'postgres',
  timezone: '+07:00',
  typeValidation: true,
  define: { freezeTableName: true, timestamps: false },
  logging: sql => {
    const date = new Date()
    date.setUTCHours(date.getUTCHours() + 7)
    console.log(
      `${color.bold}${date.toLocaleString('th')}${color.end}\n${
        color.cyan
      }${sql.replace(
        /\b([A-Z]{2,})\b/g,
        `${color.end}${color.magenta}$1${color.end}${color.cyan}`
      )}${color.end}\n`
    )
  }
})

export default sequelize
