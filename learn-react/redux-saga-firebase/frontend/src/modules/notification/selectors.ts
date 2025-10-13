import pipe from 'ramda/src/pipe'

const getState = (state: AppState) => state.notification

export const getNotifications = pipe(
  getState,
  state => state.list
)
