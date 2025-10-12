/**
 * @type {import('express').RequestHandler}
 */
function protected(req, res, next) {
  if (req.locals.user) return next()
  req.flash('error', 'Please login first!')
  return res.redirect('/login')
}

module.exports = protected
