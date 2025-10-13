import { eventChannel } from 'redux-saga'
import { takeEvery, put } from 'redux-saga/effects'

import { WatchAction, list } from './actions'
import { watchNotifications, NotifyObject } from './model'

const notificationsChannel = eventChannel(watchNotifications)

function* syncSaga(action: WatchAction<NotifyObject[]>) {
  if (action.error) {
    console.error(action)
    return yield put(action)
  }
  yield put(list.set(action.payload))
}

export default function*() {
  yield takeEvery(notificationsChannel, syncSaga)
}
