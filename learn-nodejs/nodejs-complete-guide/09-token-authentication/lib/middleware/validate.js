const { validationResult } = require('express-validator')

const AppError = require('../app-error')
const compose = require('../compose-middleware')

/**
 * @param {...import('express-validator').ValidationChain} validators
 * @returns {import('express').RequestHandler}
 */
function validate(...validators) {
  return compose(...validators, (req, _, next) => {
    const errors = validationResult(req)

    if (!errors.isEmpty()) {
      const [{ msg: message }] = errors.array()
      throw new AppError(message, 400)
    }

    return next()
  })
}

module.exports = validate
