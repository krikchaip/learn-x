import http from 'http'
import fs from 'fs'

const FILENAME = './content'

const server = http.createServer((req, res) => {
  switch (req.url) {
    case '/':
      res.setHeader('Content-Type', 'text/html')
      return res.end(`
        <html>
          <head>
            <title>Body Parsing</title>
          </head>
          <body>
            <form action="/save" method="POST">
              <input type="text" name="content" />
              <button type="submit">SEND</button>
            </form>
          </body>
        </html>
      `)
    case '/save':
      if (req.method !== 'POST') return res.end()

      let content = ''

      // ? the request itself is a readable stream. so if we want its body content,
      // ? we must listen to the 'data' event in order to get that.
      req.on('data', (/** @type {Buffer} */ buffer) => {
        content += buffer.toString()
      })

      return req.on('end', () => {
        const value = content.split('=')[1]

        fs.writeFileSync(FILENAME, value)

        res.setHeader('Content-Type', 'application/json')
        res.write(JSON.stringify({ message: 'OK' }))

        return res.end()
      })
    default:
      res.statusCode = 302
      res.statusMessage = 'REDIRECT'

      res.setHeader('Location', '/')

      return res.end()
  }
})

server.listen(3000)
