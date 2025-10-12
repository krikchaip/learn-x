import jwt from 'jsonwebtoken'

const SECRET = `
  มีสตางค์นี่นะช่างดีเหลือเกิน
  มีสตางค์จะทำอะไรก็เพลินจะตาย
  ไม่ต้องดิ้นต้องรนจะนกจะไม้
  จะเอาอะไรก็ชี้ 🎶
`

export type TokenPayload = {
  sub: string
  role?: string
}

export type VerifyOptions = {
  role?: string
}

export default {
  verify: (token: string, options?: VerifyOptions) => {
    const payload = jwt.verify(token, SECRET) as TokenPayload

    if (options?.role) {
      return payload.role === options.role ? payload : null
    }

    return payload
  },
  sign: <T extends Record<string, unknown>>(payload: T) => {
    return jwt.sign(payload, SECRET, { expiresIn: '1h' })
  }
}
