import express from 'express'
import path from 'path'

import admin from './controllers/admin.mjs'
import shop from './controllers/shop.mjs'

import './models/relations.mjs'
import User from './models/user.mjs'

import db from './utils/database.mjs'
import sequelize from './utils/sequelize.mjs'
import { __rootdir } from './utils/helpers.mjs'

const app = express()

app.set('views', path.join(__rootdir, 'views'))
app.set('view engine', 'pug')

app.use(express.urlencoded({ extended: false }))
app.use(express.static(path.join(__rootdir, 'public')))

// ? populating req.user to sequelize instance
app.use((req, _, next) => {
  // prettier-ignore
  User.findByPk(1)
    .then((user) => { req.user = user })
    .catch((err) => err)
    .finally(next)
})

app.use('/admin', admin)
app.use(shop)

app.use((_, res) => {
  res.status(404).render('404', { title: 'Page Not Found' })
})

async function bootstrap() {
  // ! not recommended in production. try migration instead
  await sequelize.sync({ alter: true })

  const server = app.listen(3000).on('close', () => {
    db.end()
    sequelize.close()
  })

  process.on('SIGINT', () => server.close())
  process.on('uncaughtException', () => server.close())
}

bootstrap()
