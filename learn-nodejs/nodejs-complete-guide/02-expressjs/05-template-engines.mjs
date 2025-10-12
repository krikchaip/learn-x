import express from 'express'
import path from 'path'
import url from 'url'

const __dirname = path.dirname(url.fileURLToPath(import.meta.url))

const app = express()

// ? related app configurations when using any template-engine
app.set('views', __dirname)
app.set('view engine', 'pug')

app.get('/', (_, res) => {
  // ? you can also pass some data to 'render' method to populate
  // ? placeholder fields on a template
  res.render('content', {
    title: 'FTW!',
    name: 'Winner',
    users: ['Jake Brown', 'Jessica Jung', 'Damian Wayne'],
  })
})

app.listen(3000)
