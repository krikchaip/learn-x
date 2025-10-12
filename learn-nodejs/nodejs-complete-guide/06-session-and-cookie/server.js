const app = require('./app')
const { connect } = require('./lib/mongodb')

;(async () => {
  const mongoose = await connect()
  const server = app.listen(3000)

  process.on('SIGINT', () => server.close())
  process.on('uncaughtException', () => server.close())

  server.on('close', () => {
    mongoose.disconnect()
  })
})()
