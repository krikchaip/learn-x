import express from 'express'
import path from 'path'
import url from 'url'

const __dirname = path.dirname(url.fileURLToPath(import.meta.url))

const app = express()

app.use(express.static(__dirname))

app.get('/', (_, res) => {
  res.sendFile(path.join(__dirname, 'index.html'))
})

app.get('/users', (_, res) => {
  res.sendFile(path.join(__dirname, 'users.html'))
})

app.listen(3000)
