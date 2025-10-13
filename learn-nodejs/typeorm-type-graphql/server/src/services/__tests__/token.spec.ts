import * as env from 'env'

import { Token } from '../token'

jest.mock('env')

it('no "SECRET" throws an error', async () => {
  const tokenService = new Token()

  // @ts-ignore: it's save here because env is now a normal object
  env.SECRET = ''

  expect(() => tokenService.sign({})).toThrowErrorMatchingInlineSnapshot(
    `"secretOrPrivateKey not found"`
  )
})

it('with "SECRET" returns a token', async () => {
  const tokenService = new Token()

  // @ts-ignore: it's save here because env is now a normal object
  env.SECRET = 'SUPER_SECRET'

  expect(tokenService.sign({ name: 'winner' })).toEqual(expect.any(String))
})
