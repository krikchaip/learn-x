import { combineReducers } from 'redux'

import auth from './auth'
import project from './project'
import notification from './notification'

export default combineReducers({
  auth: auth.reducer,
  project: project.reducer,
  notification: notification.reducer
})
