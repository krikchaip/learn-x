const qs = require('querystring')

/**
 * [Redirect while pass something to another route](https://stackoverflow.com/questions/19035373/how-do-i-redirect-in-expressjs-while-passing-some-context)
 *
 * @param {Object} options
 * @param {String} options.path
 * @param {Boolean} [options.params]
 *
 * @returns {import('express').RequestHandler}
 */
function redirect(options) {
  return function (req, res, next) {
    let resultURL = options.path + '?'

    if (options.params) {
      resultURL += qs.stringify(req.params)
    }

    return res.redirect(resultURL)
  }
}

module.exports = redirect
