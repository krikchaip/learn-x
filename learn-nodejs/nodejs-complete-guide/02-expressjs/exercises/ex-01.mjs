import express from 'express'

const app = express()

// app.use((_, res, next) => {
//   res.removeHeader('X-Powered-By')
//   return next()
// })

app.set('x-powered-by', false)

// ! if we instead come up with '/' first, then we'll never reach this middleware
// ! because `app.use` match all paths starts with '/'
app.use('/users', (req, res) => {
  console.log(req.path)
  res.send('The Users')
})

app.use('/', (req, res) => {
  console.log(req.path)
  res.send('The Root')
})

app.listen(3000)
