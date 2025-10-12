import express from 'express'
import path from 'path'

import admin from './controllers/admin.mjs'
import shop from './controllers/shop.mjs'

import User from './models/user.mjs'

import { connect as dbConnect } from './utils/database.mjs'
import { connect as odmConnect } from './utils/mongoose.mjs'
import { __rootdir } from './utils/helpers.mjs'

const app = express()

app.set('views', path.join(__rootdir, 'views'))
app.set('view engine', 'pug')

app.use(express.urlencoded({ extended: false }))
app.use(express.static(path.join(__rootdir, 'public')))

// ? populating req.user to the user in the database
app.use(async (req, _, next) => {
  req.user = await User.findById('5f2fda506008bef9c5d2ef7b')
  next()
})

app.use('/admin', admin)
app.use(shop)

app.use((_, res) => {
  res.status(404).render('404', { title: 'Page Not Found' })
})

async function bootstrap() {
  const dbConnection = await dbConnect()
  const odmConnection = await odmConnect()

  const server = app.listen(3000)

  process.on('SIGINT', () => server.close())
  process.on('uncaughtException', () => server.close())

  server.on('close', () => {
    dbConnection.close()
    odmConnection.disconnect()
  })
}

bootstrap()
