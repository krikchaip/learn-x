const jwt = require('jsonwebtoken')
const middleware = require('express-async-handler')

const AppError = require('../app-error')

const SECRET = `
  มีสตางค์นี่นะช่างดีเหลือเกิน
  มีสตางค์จะทำอะไรก็เพลินจะตาย
  ไม่ต้องดิ้นต้องรนจะนกจะไม้
  จะเอาอะไรก็ชี้ 🎶
`

/**
 * @type {import('express').RequestHandler}
 */
async function verify(req, res, next) {
  /** @type {String} */
  let token = req.query.bearer

  if (req.get('Authorization') && !token) {
    const [type, creds] = req.get('Authorization').split(' ')

    if (!/^bearer$/i.test(type) || !creds)
      return next(new AppError('Authorization header malformed!', 400))

    token = creds
  }

  if (!token) return next(new AppError('Token not found!', 404))

  jwt.verify(token, SECRET, null, (err, data) => {
    if (err) return next(err)
    req.token = data
    return next()
  })
}

/**
 * @template {Record<String, any>} T
 * @param {T} payload
 * @returns {Promise<String>} signed JWT token
 */
async function sign(payload) {
  /** @type {import('jsonwebtoken').SignOptions} */
  const options = { expiresIn: '1h' }

  return new Promise((res, rej) => {
    jwt.sign(payload, SECRET, options, (err, token) => {
      if (err) return rej(err)
      return res(token)
    })
  })
}

module.exports = {
  verify: middleware(verify),
  sign
}
