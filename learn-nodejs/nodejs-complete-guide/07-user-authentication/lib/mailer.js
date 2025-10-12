const nodemailer = require('nodemailer')

/** @type {import('nodemailer').Transporter} */
let transporter

async function createTransporter() {
  const account = await nodemailer.createTestAccount()
  return nodemailer.createTransport({
    host: 'smtp.ethereal.email',
    port: 587,
    secure: false,
    auth: { user: account.user, pass: account.pass }
  })
}

module.exports = {
  /**
   * @param {import('nodemailer').SendMailOptions} options
   * @returns {Promise<String>} a link to the stored message on Ethereal
   */
  send: async options => {
    if (!transporter) transporter = await createTransporter()
    const info = await transporter.sendMail(options)
    return nodemailer.getTestMessageUrl(info)
  }
}
