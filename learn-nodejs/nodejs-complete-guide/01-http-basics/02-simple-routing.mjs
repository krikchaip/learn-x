import http from 'http'

const server = http.createServer((req, res) => {
  res.setHeader('Content-Type', 'text/html')

  switch (req.url) {
    case '/':
      return res.end(`
        <html>
          <head>
            <title>Simple Routing</title>
          </head>
          <body>
            <h1>Hello World!</h1>
          </body>
        </html>
      `)
    default:
      return res.end(`
        <html>
          <head>
            <title>Simple Routing</title>
          </head>
          <body>
            <h1>Hello from ${req.url.replace('/', '')}!</h1>
          </body>
        </html>
      `)
  }
})

server.listen(3000)
