const express = require('express')
const { body } = require('express-validator')
const middleware = require('express-async-handler')

const User = require('../model/user')

const ack = require('../lib/app-ack')
const data = require('../lib/app-data')

const token = require('../lib/middleware/token')
const validate = require('../lib/middleware/validate')

const app = express.Router()

app.post(
  '/signup',
  validate(
    body('email', 'Please enter an email with the right scheme!')
      .exists({ checkFalsy: true })
      .isEmail()
      .custom(async email => {
        if (!(await User.findOne({ email }))) return
        throw 'Email already exists!'
      }),
    body('password', 'Please enter a password with at least 5 characters!')
      .exists({ checkFalsy: true })
      .isLength({ min: 5 }),
    body('name', 'Please enter a name!').exists({ checkFalsy: true })
  ),
  middleware(async (req, res) => {
    await new User(req.body).save()
    return res.status(201).json(ack('A new user was created.'))
  })
)

app.post(
  '/login',
  validate(
    body('email').custom(async (email, { req }) => {
      const { password } = req.body
      const user = await User.findOne({ email })

      if (!user || !(await user.matchPassword(password)))
        throw "Wrong password or email doesn't exists!"

      req.user = user
    })
  ),
  middleware(async (req, res) => {
    const jwt = await token.sign({ iss: req.user.id })
    return res.json(data({ token: jwt }))
  })
)

module.exports = app
