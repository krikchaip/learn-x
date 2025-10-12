/**
 * @type {import('express').RequestHandler}
 */
function cors(req, res, next) {
  // prettier-ignore
  res.set({
    'Access-Control-Allow-Origin': ['http://localhost:3001'],
    'Access-Control-Allow-Headers':
      ['Content-Type', 'Authorization'].join(', '),
    'Access-Control-Allow-Methods': ['PATCH'].join(', ')
  })

  return next()
}

module.exports = cors
