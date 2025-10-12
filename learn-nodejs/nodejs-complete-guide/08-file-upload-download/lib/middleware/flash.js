const path = require('path')

const { validationResult } = require('express-validator')

const compose = require('../compose-middleware')

/**
 * @param {...import('express-validator').ValidationChain} validators
 * @returns {import('express').RequestHandler}
 */
function flash(...validators) {
  return compose(...validators, function (req, res, next) {
    const errors = validationResult(req)

    if (!errors.isEmpty()) {
      const [{ msg: error }] = errors.array()
      const route = path.join(req.baseUrl, req.path).slice(1) || 'home'
      return res.render(route, { ...req.body, error })
    }

    return next()
  })
}

module.exports = flash
