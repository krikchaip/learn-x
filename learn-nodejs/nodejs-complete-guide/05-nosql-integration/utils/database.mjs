import mongodb from 'mongodb'

const { MongoClient } = mongodb

const config = {
  url: 'mongodb://localhost:27017',
  database: 'nodejs-complete-guide',
}

const client = new MongoClient(config.url, { useUnifiedTopology: true })

const db = new Proxy(
  { _ref: null },
  {
    get(target, key) {
      if (key in target) return target[key]
      if (target._ref) return target._ref.collection(key)
      else throw new Error('Connection Failed...')
    },
    set(target, key, value) {
      if (key !== '_ref') return false
      target._ref = value
      return true
    },
  }
)

export async function connect() {
  await client.connect()
  db._ref = client.db(config.database)
  return client
}

export default db
