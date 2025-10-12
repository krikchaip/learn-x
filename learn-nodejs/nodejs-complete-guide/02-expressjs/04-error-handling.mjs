import express from 'express'

const app = express()

// ? always generate an error
app.use(() => {
  throw new Error()
})

// ? error handling middleware
// ! ALWAYS PLACE AN ERROR MIDDLEWARE THE LAST
app.use((err, req, res, next) => {
  res.status(501).send(`
    <p>An error occured</p>
  `)
})

app.listen(3000)
