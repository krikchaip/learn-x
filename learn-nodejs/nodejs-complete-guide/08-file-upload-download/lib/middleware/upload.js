const path = require('path')
const util = require('util')

const multer = require('multer')

const uploads = multer({
  storage: multer.diskStorage({
    destination: util.callbackify(async () => 'static'),
    filename: util.callbackify(async (_, file) => {
      const { name, ext } = path.parse(file.originalname)
      return `${name}__${Date.now()}${ext}`
    })
  }),
  fileFilter: util.callbackify(async (_, file) => {
    if (!file.mimetype.startsWith('image'))
      throw new Error('An avatar must be an image!')

    return true
  })
})

module.exports = uploads
