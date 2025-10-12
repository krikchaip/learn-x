const qs = require('querystring')
const crypto = require('crypto')

const express = require('express')
const middleware = require('express-async-handler')

const mailer = require('../lib/mailer')
const protected = require('../lib/middleware/protected')

const User = require('../model/user')

const APP_URL = 'http://localhost:3000'

const app = express.Router()

app.post('/forget-password', middleware(async (req, res) => {
  const user = await User.findOne({ email: req.body.email })

  if (!user) {
    req.flash('error', 'Email not found!')
    return res.redirect('/forget-password')
  }

  const token = crypto.randomBytes(24).toString('base64')
  const encodedToken = qs.stringify({ token })

  user.token.passwordReset = token
  user.token.passwordResetExpire = new Date(Date.now() + 1000 * 3600)
  await user.save()

  const link = await mailer.send({
    from: 'test@nodejs-complete-guide.xyz',
    to: user.email,
    subject: 'Password Reset 🔑',
    html: `
      <p>
        You requested a password reset.
        Please proceed before ${user.token.passwordResetExpire.toString()}.
      </p>
      <p>
        Click
        <a href="${APP_URL}/reset-password?${encodedToken}">here</a>
        to set a new password.
      </p>
    `
  })

  req.flash('error', `Click <a href="${link}">here</a> to check your email!`)
  return res.redirect('/forget-password')
}))

app.post('/reset-password', middleware(async (req, res) => {
  const { token, password } = req.body
  const user = await User.findOne({
    'token.passwordReset': token,
    'token.passwordResetExpire': { $gt: new Date() }
  })

  if (!user) {
    req.flash('error', 'An error occured!')
    return res.redirect('/reset-password')
  }

  user.password = password
  user.token.passwordReset = undefined
  user.token.passwordResetExpire = undefined
  await user.save()

  return res.redirect('/login')
}))

app.post('/edit-profile', protected, middleware(async (req, res) => {
  Object.assign(req.locals.user, req.body)
  await req.locals.user.save()
  return res.redirect('/')
}))

module.exports = app
