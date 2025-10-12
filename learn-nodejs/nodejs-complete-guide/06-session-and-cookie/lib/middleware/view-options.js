const $ = require('express-async-handler')

const User = require('../../model/user')

/**
 * @param {Express.Request} req
 * @param {Express.Response} res
 * @param {import('express').NextFunction} next
 */
async function viewOptions(req, res, next) {
  const { uid } = req.session
  const user = uid ? await User.findById(uid) : null

  req.user = user
  req.viewOptions = {
    authenticated: user ? !!user : false,
    user: user ? user.toJSON() : null,
  }

  return next()
}

module.exports = $(viewOptions)
