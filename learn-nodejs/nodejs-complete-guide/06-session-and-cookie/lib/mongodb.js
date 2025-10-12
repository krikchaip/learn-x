const mongoose = require('mongoose')

const config = {
  url: 'mongodb://localhost:27017',
  database: 'nodejs-complete-guide',
}

exports.connect = async function () {
  return mongoose.connect(`${config.url}/${config.database}`, {
    useNewUrlParser: true,
    useUnifiedTopology: true,
    useFindAndModify: false,
  })
}
