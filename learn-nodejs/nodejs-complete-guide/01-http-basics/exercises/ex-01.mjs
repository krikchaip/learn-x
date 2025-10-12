import http from 'http'

const users = []

const server = http.createServer((req, res) => {
  switch (req.url) {
    case '/':
      res.setHeader('Content-Type', 'text/html')

      return res.end(`
        <html>
          <head>
            <title>Assignment 1</title>
          </head>
          <body>
            <form method="POST" action="/create-user">
              <input name="username" />
              <button type="submit">CREATE</button>
            </form>
          </body>
        </html>
      `)
    case '/users':
      res.setHeader('Content-Type', 'text/html')

      return res.end(`
        <html>
          <head>
            <title>Assignment 1</title>
          </head>
          <body>
            <ul>
              ${users.map((u) => `<li>${u}</li>`).join('')}
            </ul>
          </body>
        </html>
      `)
    case '/create-user':
      if (req.method !== 'POST') return res.end()

      let content = ''

      req.on('data', (buf) => {
        content += buf.toString()
      })

      return req.on('end', () => {
        const value = content.split('=')[1]

        console.log(value)
        users.push(value)

        return res.end()
      })
  }
})

server.listen(3000)
