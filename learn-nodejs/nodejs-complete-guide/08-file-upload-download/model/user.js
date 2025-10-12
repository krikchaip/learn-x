const { Schema, model } = require('mongoose')
const bcrypt = require('bcrypt')

const User = new Schema(
  {
    email: { type: String, required: true, unique: true },
    password: { type: String, required: true },
    firstname: String,
    lastname: String,
    age: Number,
    sex: { type: String, enum: ['', 'male', 'female'] },
    avatar: String
  },
  { versionKey: false, toJSON: { virtuals: true } }
)

User.virtual('name')
  .get(function () {
    return `${this.firstname} ${this.lastname}`
  })
  .set(function (/** @type {String} */ value) {
    const [firstname, lastname] = value.trim().split(' ')

    this.firstname = firstname
    this.lastname = lastname
  })

// reference: https://stackoverflow.com/questions/14588032/mongoose-password-hashing
User.pre('save', async function () {
  if (!this.isModified('password')) return
  this.password = await bcrypt.hash(this.password, 10)
})

User.method('matchPassword', async function (password) {
  return bcrypt.compare(password, this.password)
})

module.exports = model('User', User, 'user')
