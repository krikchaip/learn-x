const mongoose = require('mongoose')

const config = {
  url: 'mongodb://localhost:27017',
  database: 'nodejs-complete-guide'
}

async function connect() {
  return mongoose.connect(`${config.url}/${config.database}`, {
    useNewUrlParser: true,
    useUnifiedTopology: true,
    useCreateIndex: true,
    useFindAndModify: false
  })
}

module.exports = {
  connect
}
