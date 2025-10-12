import http from 'http'

const server = http.createServer((req, res) => {
  console.log(req.url, req.method)
  console.log(req.headers)

  res.setHeader('Content-Type', 'application/json')

  res.write(JSON.stringify({ content: 'HELLO WORLD' }))
  res.end() // ! MUST CALL EVERYTIME

  // ? alternative to above
  // res.end(JSON.stringify({ content: 'HELLO WORLD' }))
})

server.listen(3000)
