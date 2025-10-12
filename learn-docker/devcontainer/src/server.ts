import express from 'express'
import { createClient } from 'redis'

import { PORT, REDIS_URL } from '~/config'

const app = express()
const redisClient = createClient({ url: REDIS_URL })

app.get('/', async (req, res) => {
  const visits = await redisClient.incr('visits')
  res.send(`<h1>Number of visits: ${visits}</h1>`)
})

app.get('/crash', (req, res) => {
  process.exit(1)
})

app.listen(PORT, async () => {
  redisClient.on('error', console.error)
  await redisClient.connect()

  if (!redisClient.isReady) {
    console.error(`redis connection failed!`)
    return process.exit(1)
  }

  console.log(`connected to redis-server at ${REDIS_URL}`)
  console.log(`server is listening at port ${PORT}`)
})
