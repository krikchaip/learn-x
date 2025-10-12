import mongoose from 'mongoose'

const config = {
  url: 'mongodb://localhost:27017',
  database: 'nodejs-complete-guide',
}

export async function connect() {
  return mongoose.connect(`${config.url}/${config.database}`, {
    useNewUrlParser: true,
    useUnifiedTopology: true,
  })
}
