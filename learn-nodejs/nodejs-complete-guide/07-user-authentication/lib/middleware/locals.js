const middleware = require('express-async-handler')

const User = require('../../model/user')

/**
 * @type {import('express').RequestHandler}
 */
async function locals(req, res, next) {
  const { uid } = req.session
  const user = uid ? await User.findById(uid) : null

  req.locals = { user }

  res.locals.error = req.flash('error')[0]
  res.locals._csrf = req.csrfToken()
  res.locals.authenticated = user ? !!user : false
  res.locals.user = user ? user.toJSON() : null

  return next()
}

module.exports = middleware(locals)
