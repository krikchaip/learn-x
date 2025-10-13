import { fork } from 'redux-saga/effects'

import auth from './auth'
import project from './project'
import notification from './notification'

export default function*() {
  yield fork(auth.saga)
  yield fork(project.saga)
  yield fork(notification.saga)
}
