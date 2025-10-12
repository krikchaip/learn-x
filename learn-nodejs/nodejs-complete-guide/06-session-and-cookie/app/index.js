const path = require('path')
const express = require('express')
const session = require('express-session')
const $ = require('express-async-handler')
const mongoose = require('mongoose')
const MongoStore = require('connect-mongo')(session)

const User = require('../model/user')
const viewOptions = require('../lib/middleware/view-options')
const protected = require('../lib/middleware/protected')

const app = express()

app.set('views', __dirname)
app.set('view engine', 'pug')

app.use(express.urlencoded({ extended: false }))
app.use(express.static(path.join(__dirname, '..', 'public')))
app.use(
  session({
    secret: 'nodejs-complete-guide',
    resave: false,
    saveUninitialized: false,
    store: new MongoStore({
      mongooseConnection: mongoose.connection,
      collection: 'session',
      stringify: false,
    }),
  })
)

app.use(viewOptions)

app.get('/', (req, res) => {
  return res.render('home', req.viewOptions)
})

app.get('/edit-profile', protected, (req, res) => {
  return res.render('edit-profile', req.viewOptions)
})

app.post(
  '/edit-profile',
  protected,
  $(async (req, res) => {
    const { age, sex, bio } = req.body

    req.user.age = age
    req.user.sex = sex
    req.user.bio = bio

    await req.user.save()

    return res.redirect('/')
  })
)

app.get('/login', (req, res) => {
  return res.render('login', req.viewOptions)
})

app.post(
  '/login',
  $(async (req, res) => {
    const payload = {
      firstname: req.body.firstname,
      lastname: req.body.lastname,
    }

    const user = await User.findOneAndUpdate(
      payload,
      { $set: payload },
      { upsert: true, new: true }
    )

    req.session.uid = user.id
    req.session.save(() => {
      if (['age', 'sex', 'bio'].some((field) => !user[field])) {
        return res.redirect('/edit-profile')
      }

      return res.redirect('/')
    })
  })
)

app.get('/logout', protected, (req, res) => {
  req.session.destroy(() => {
    req.user = null
    res.redirect('/login')
  })
})

module.exports = app
