import express from 'express'

const app = express()
const users = []

// ? because HTML form sends 'application/x-www-form-urlencoded'
// ? body by default. thus we have to use this middleware
app.use(express.urlencoded({ extended: false }))

app.get('/', (_, res) => {
  return res.send(`
    <form action="/create-user" method="POST">
      <input name="user" />
      <button type="submit">CREATE</button>
    </form>
    <ul>
      ${users.map((u) => `<li>${u}</li>`).join('')}
    </ul>
  `)
})

app.post('/create-user', (req, res) => {
  // ! without body-parser related middlewares.
  // ! 'req.body' will be 'undefined' by default
  const { user } = req.body

  users.push(user)
  res.redirect('/')
})

app.listen(3000)
