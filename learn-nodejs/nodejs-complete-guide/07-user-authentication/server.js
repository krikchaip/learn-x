const app = require('./app')
const db = require('./lib/mongodb')

async function bootstrap() {
  const client = await db.connect()
  const server = app.listen(3000)

  process.on('SIGINT', () => server.close())
  process.on('uncaughtException', () => server.close())

  server.on('close', () => {
    client.disconnect()
  })
}

bootstrap()
