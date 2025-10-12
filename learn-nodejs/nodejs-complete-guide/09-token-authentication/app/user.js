const express = require('express')
const { body } = require('express-validator')
const middleware = require('express-async-handler')

const User = require('../model/user')

const ack = require('../lib/app-ack')
const data = require('../lib/app-data')

const token = require('../lib/middleware/token')
const validate = require('../lib/middleware/validate')

const app = express.Router()

app.patch(
  '/edit',
  token.verify,
  validate(
    body().custom(async (data = {}) => {
      // Check whether if an object is empty.
      // ref: https://stackoverflow.com/questions/679915/how-do-i-test-for-an-empty-javascript-object
      if (
        (Object.keys(data).length === 0 && data.constructor === Object) ||
        Object.keys(data).every(prop => !data[prop])
      )
        throw 'There is no property to change!'

      if (data.email || data.password) throw 'Invalid payload property!'
    })
  ),
  middleware(async (req, res) => {
    const user = await User.findById(req.token.iss)

    Object.assign(user, req.body)
    await user.save()

    return res.json(ack('Your data is updated.'))
  })
)

module.exports = app
