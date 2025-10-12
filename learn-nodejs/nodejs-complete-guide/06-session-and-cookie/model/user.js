const { Schema, model } = require('mongoose')

const User = new Schema(
  {
    firstname: String,
    lastname: String,
    age: Number,
    sex: { type: String, enum: ['', 'male', 'female'] },
    bio: String,
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

module.exports = model('User', User, 'user')
