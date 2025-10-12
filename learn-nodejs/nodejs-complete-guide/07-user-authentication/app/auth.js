const express = require('express')
const middleware = require('express-async-handler')

const protected = require('../lib/middleware/protected')

const User = require('../model/user')

const app = express.Router()

app.post('/login', middleware(async (req, res) => {
  const user = await User.findOne({ email: req.body.email })

  if (!user || !(await user.matchPassword(req.body.password))) {
    req.flash('error', "Wrong password or email doesn't exist!")
    return res.redirect('/login')
  }

  req.session.uid = user.id

  req.session.save(() => {
    if (['age', 'sex', 'bio'].some(field => !user[field])) {
      return res.redirect('/edit-profile')
    }

    return res.redirect('/')
  })
}))

app.get('/logout', protected, (req, res) => {
  req.session.destroy(() => {
    req.locals = {}
    res.locals = {}
    return res.redirect('/login')
  })
})

app.post('/signup', middleware(async (req, res) => {
  if (await User.findOne({ email: req.body.email })) {
    req.flash('error', 'Email already exists!')
    return res.redirect('/signup')
  }

  try {
    await new User(req.body).save()
    return res.redirect('/login')
  } catch (error) {
    req.flash('error', JSON.stringify(error))
    return res.redirect('/signup')
  }
}))

module.exports = app
