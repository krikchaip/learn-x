/**
 * @param {String} path
 * @returns {import('express').RequestHandler}
 */
function render(path) {
  return function (req, res, next) {
    return res.render(path)
  }
}

module.exports = render
