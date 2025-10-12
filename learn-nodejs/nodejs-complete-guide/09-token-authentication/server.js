const app = require('./app')
const view = require('./view')

const db = require('./lib/mongodb')

async function bootstrap() {
  const client = await db.connect()
  const server = app.listen(3000)
  const vserver = view.listen(3001)

  function onClose() {
    server.close()
    vserver.close()
  }

  process.on('SIGINT', onClose)
  process.on('uncaughtException', onClose)

  server.on('close', () => {
    client.disconnect()
  })
}

bootstrap()
