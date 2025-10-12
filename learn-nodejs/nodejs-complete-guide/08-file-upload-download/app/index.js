const express = require('express')
const session = require('express-session')
const mongoose = require('mongoose')
const store = require('connect-mongo')
const flash = require('connect-flash')
const csurf = require('csurf')

const routes = require('./routes')
const auth = require('./auth')
const user = require('./user')

const locals = require('../lib/middleware/locals')
const error = require('../lib/middleware/error')

const MongoStore = store(session)

const app = express()

app.set('views', 'view')
app.set('view engine', 'pug')

app.use(express.static('public'))
app.use(express.urlencoded({ extended: false }))
app.use(session({
  secret: 'nodejs-complete-guide',
  resave: false,
  saveUninitialized: false,
  store: new MongoStore({
    mongooseConnection: mongoose.connection,
    collection: 'session',
    stringify: false
  })
}))
app.use(csurf())
app.use(flash())

app.use(locals)

app.use('/auth', auth)
app.use('/user', user)
app.use('/', routes)

app.use(error)

module.exports = app
