const path = require('path')

/**
 * @type {import('express').ErrorRequestHandler}
 */
function error(err, req, res, next) {
  if (res.headersSent) return next(err)
  const route = path.join(req.baseUrl, req.path).slice(1) || 'home'
  return res.render(route, { ...req.body, error: err.message })
}

module.exports = error
