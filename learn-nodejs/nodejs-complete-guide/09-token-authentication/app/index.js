const express = require('express')

const auth = require('./auth')
const user = require('./user')

const cors = require('../lib/middleware/cors')
const error = require('../lib/middleware/error')

const app = express()

app.use(cors)
app.use(express.json())

app.use('/auth', auth)
app.use('/user', user)

app.use(error)

module.exports = app
