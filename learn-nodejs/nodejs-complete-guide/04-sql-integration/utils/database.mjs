import mysql from 'mysql2'

const db = mysql.createPool({
  host: 'localhost',
  user: 'root',
  password: 'root',
  database: 'nodejs-complete-guide',
})

export default db
