import { Service } from 'typedi'
import { sign, SignOptions, JsonWebTokenError } from 'jsonwebtoken'

import { SECRET } from 'env'

@Service()
export class Token {
  sign(payload: Record<string, unknown>, options?: SignOptions) {
    if (!SECRET) throw new JsonWebTokenError('secretOrPrivateKey not found')
    return sign(payload, SECRET, options)
  }
}
