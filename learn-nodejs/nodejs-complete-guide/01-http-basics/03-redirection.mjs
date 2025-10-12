import http from 'http'

const server = http.createServer((req, res) => {
  res.setHeader('Content-Type', 'text/html')

  switch (req.url) {
    case '/':
      return res.end(`
        <html>
          <head>
            <title>HTTP Redirection</title>
          </head>
          <body>
            <h1>You come to the right place.</h1>
          </body>
        </html>
      `)
    default:
      res.statusCode = 302
      res.statusMessage = 'FUCK OFF'

      res.setHeader('Location', '/')

      return res.end()
  }
})

server.listen(3000)
