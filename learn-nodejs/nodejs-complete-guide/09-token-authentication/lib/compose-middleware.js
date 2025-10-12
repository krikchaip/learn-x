/**
 * [How to compose ExpressJS middlwares](https://stackoverflow.com/questions/20274483/how-do-i-combine-connect-middleware-into-one-middleware)
 *
 * @param {...import('express').RequestHandler} middlewares
 * @returns {import('express').RequestHandler}
 */
function compose(...middlewares) {
  return middlewares.reduce(
    (f, g) =>
      function (req, res, next) {
        f(req, res, function (value) {
          if (value) return next(value)
          return g(req, res, next)
        })
      }
  )
}

module.exports = compose
