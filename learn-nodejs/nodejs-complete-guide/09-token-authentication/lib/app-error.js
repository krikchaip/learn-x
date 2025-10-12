class AppError extends Error {
  /**
   * @param {String} message
   * @param {Number} [code]
   */
  constructor(message, code = 500) {
    super(message)
    this.type = 'error'
    this.code = code
    this.payload = { message }
  }
}

module.exports = AppError
