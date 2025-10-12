const AppError = require('../app-error')

/**
 * @type {import('express').ErrorRequestHandler}
 */
function error(err, _, res, next) {
  if (res.headersSent) return next(err)
  if (!(err instanceof AppError))
    err = new AppError(err.message || 'Some error occured!')

  return res.status(err.code).json(err)
}

module.exports = error
