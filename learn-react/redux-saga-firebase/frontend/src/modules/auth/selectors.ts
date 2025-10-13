import pipe from 'ramda/src/pipe'

const getState = (state: AppState) => state.auth

export const getIsLoggedIn = pipe(
  getState,
  state => state.isLoggedIn
)

export const getError = pipe(
  getState,
  state => state.error
)

export const getUser = pipe(
  getState,
  state => state.user
)

export const getUserInitials = pipe(
  getUser,
  user => {
    if (!user) return '...'
    return user.profile.firstname[0] + user.profile.lastname[0]
  }
)
