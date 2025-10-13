import { Credentials, SignupForm, User } from './model'

export type Action =
  | InitAction
  | SignupAction
  | SigninAction
  | SignoutAction
  | FlushAction

export type InitAction =
  | ReturnType<typeof init.isLoggedIn>
  | ReturnType<typeof init.user>
export const init = {
  isLoggedIn: (value: boolean) => ({
    type: '@auth/init:isLoggedIn' as const,
    payload: value
  }),
  user: (user: User | null) => ({
    type: '@auth/init:user' as const,
    payload: user
  })
}

export type SignupAction =
  | ReturnType<typeof signup.attempt>
  | ReturnType<typeof signup.success>
  | ReturnType<typeof signup.error>
export const signup = {
  attempt: (form: SignupForm) => ({
    type: '@auth/signup:attempt' as const,
    payload: form
  }),
  success: (user: User | null, isLoggedIn: boolean) => ({
    type: '@auth/signup:success' as const,
    payload: { user, isLoggedIn }
  }),
  error: (err: Error) => ({
    type: '@auth/signup:error' as const,
    payload: err,
    error: true as const
  })
}

export type SigninAction =
  | ReturnType<typeof signin.attempt>
  | ReturnType<typeof signin.success>
  | ReturnType<typeof signin.error>
export const signin = {
  attempt: (creds: Credentials) => ({
    type: '@auth/signin:attempt' as const,
    payload: creds
  }),
  success: (user: User | null, isLoggedIn: boolean) => ({
    type: '@auth/signin:success' as const,
    payload: { user, isLoggedIn }
  }),
  error: (err: Error) => ({
    type: '@auth/signin:error' as const,
    payload: err,
    error: true as const
  })
}

export type SignoutAction =
  | ReturnType<typeof signout.attempt>
  | ReturnType<typeof signout.success>
export const signout = {
  attempt: () => ({
    type: '@auth/signout:attempt' as const
  }),
  success: () => ({
    type: '@auth/signout:success' as const
  })
}

export type FlushAction = ReturnType<typeof flush.error>
export const flush = {
  error: () => ({
    type: '@auth/flush:error' as const
  })
}
