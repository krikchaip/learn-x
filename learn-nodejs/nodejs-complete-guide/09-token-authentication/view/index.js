const express = require('express')

const render = require('../lib/middleware/render')

const app = express()

app.set('views', 'view')
app.set('view engine', 'pug')

app.get('/', render('home'))

module.exports = app
