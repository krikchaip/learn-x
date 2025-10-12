import express from 'express'
import path from 'path'
import url from 'url'

const __dirname = path.dirname(url.fileURLToPath(import.meta.url))

const app = express()
const users = []

app.set('views', __dirname)
app.set('view engine', 'pug')

app.use(express.urlencoded({ extended: false }))

app.get('/', (_, res) => {
  res.render('index')
})

app.get('/users', (_, res) => {
  res.render('users', { users })
})

app.post('/users', (req, res) => {
  const { user } = req.body
  users.push(user)
  return res.redirect('/')
})

app.listen(3000)
