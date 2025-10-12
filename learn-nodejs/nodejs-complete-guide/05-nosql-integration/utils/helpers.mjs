import path from 'path'
import url from 'url'

export const __rootdir = path.resolve(
  path.dirname(url.fileURLToPath(import.meta.url)),
  '..'
)
