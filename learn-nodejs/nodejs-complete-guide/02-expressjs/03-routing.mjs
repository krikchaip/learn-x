import express from 'express'

const app = express()

// ? Router is just an express mini app.
// ? it can have its own middlewares & routes
const admin = express.Router()
const shop = express.Router()

app.use(express.urlencoded({ extended: false }))

app.get('/', (_, res) => {
  return res.send(`Hello World`)
})

admin.get('/', (_, res) => {
  return res.send(`Hello from Admin`)
})

shop.get('/', (_, res) => {
  return res.send(`Hello from Shop`)
})

app.use('/admin', admin)
app.use('/shop', shop)

app.listen(3000)
