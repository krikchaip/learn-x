const { Schema, model } = require('mongoose')
const bcrypt = require('bcrypt')

const User = new Schema(
  {
    email: { type: String, required: true, unique: true },
    password: { type: String, required: true },
    name: { type: String, required: true }
  },
  { versionKey: false }
)

User.pre('save', async function () {
  if (!this.isModified('password')) return
  this.password = await bcrypt.hash(this.password, 10)
})

User.method('matchPassword', async function (password) {
  return bcrypt.compare(password, this.password)
})

module.exports = model('User', User, 'user')
