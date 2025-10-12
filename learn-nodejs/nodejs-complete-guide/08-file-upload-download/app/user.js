const path = require('path')
const fs = require('fs/promises')

const express = require('express')
const { body } = require('express-validator')
const middleware = require('express-async-handler')

const upload = require('../lib/middleware/upload')
const flash = require('../lib/middleware/flash')
const protected = require('../lib/middleware/protected')

const User = require('../model/user')

const app = express.Router()

app.use(
  '/static',
  protected,
  middleware(async (req, res, next) => {
    const avatar = path.join(req.baseUrl, req.path)
    const user = await User.findById(req.locals.user.id)

    if (user.avatar === avatar) return next()

    req.flash('error', 'Not authorized!')
    return res.redirect('/')
  }),
  express.static('static')
)

app.post(
  '/edit-profile',
  protected,
  upload.single('avatar'),
  flash(
    body('age')
      .optional({ checkFalsy: true })
      .isInt({ gt: 0 })
      .withMessage('Age must be at least 1!')
  ),
  middleware(async (req, res) => {
    let { avatar } = req.locals.user

    if (req.file) {
      fs.unlink(avatar.replace(/^(\/user\/)/, ''))
      avatar = `/user/${req.file.path}`
    }

    Object.assign(req.locals.user, req.body, { avatar })
    await req.locals.user.save()
    return res.redirect('/')
  })
)

module.exports = app
