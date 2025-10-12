const express = require('express')
const { body } = require('express-validator')
const middleware = require('express-async-handler')

const upload = require('../lib/middleware/upload')
const flash = require('../lib/middleware/flash')
const protected = require('../lib/middleware/protected')

const User = require('../model/user')

const app = express.Router()

app.post('/login', middleware(async (req, res) => {
  const user = await User.findOne({ email: req.body.email })

  if (!user || !(await user.matchPassword(req.body.password))) {
    throw new Error("Wrong password or email doesn't exist!")
  }

  req.session.uid = user.id
  req.session.save(() => {
    if (['age', 'sex', 'avatar'].some(field => !user[field])) {
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

app.post(
  '/signup',
  upload.single('avatar'),
  flash(
    body('email', 'Please enter an email with the right email scheme!')
      .exists()
      .isEmail()
      .custom(async value => {
        if (!(await User.findOne({ email: value }))) return
        throw new Error('Email already exists!')
      }),
    body('password', 'Please enter a password with at least 5 characters!')
      .exists()
      .isLength({ min: 5 }),
    body('confirm_password').custom(async (value, { req }) => {
      if (value === req.body.password) return
      throw new Error('Password mismatch!')
    }),
    body('age')
      .optional({ checkFalsy: true })
      .isInt({ gt: 0 })
      .withMessage('Age must be at least 1!')
  ),
  middleware(async (req, res) => {
    const avatar = req.file ? `/user/${req.file.path}` : undefined
    await new User({...req.body, avatar }).save()
    return res.redirect('/login')
  })
)

module.exports = app
