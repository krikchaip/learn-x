/**
 * @param {Express.Request} req
 * @param {Express.Response} res
 * @param {import('express').NextFunction} next
 */
async function protected(req, res, next) {
  if (req.user) return next()
  else return res.redirect('/login')
}

module.exports = protected
