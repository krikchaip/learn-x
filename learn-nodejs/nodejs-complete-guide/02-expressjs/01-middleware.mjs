import express from 'express'

const app = express()

app.use('/', (req, res, next) => {
  console.log('Middleware 1')

  // ? allowing request to continue to the next middleware stack
  return next()
})

app.use('/', (req, res, next) => {
  console.log('Middleware 2')

  // ? default -> Content-Type: text/html
  return res.send(`
    <p>Hello from express!</p>
  `)
})

app.listen(3000)
