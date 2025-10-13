import firebase from 'services/firebase'
import cuid from 'cuid'

import { WatchAction } from './actions'

export interface NotifyObject {
  id: string
  name: string
  detail: string
  time: Date
}

export type NotificationWatcher = (value: WatchAction<NotifyObject[]>) => void

export function watchNotifications(watcher: NotificationWatcher) {
  const fromSnapshot = (snap: firebase.firestore.QuerySnapshot) =>
    snap.docs.map(s => {
      const data = s.data()
      return {
        id: cuid(),
        name: data.who.name,
        detail: data.detail,
        time: data.time.toDate()
      } as NotifyObject
    })

  const unsubscribe = firebase
    .firestore()
    .collection('notifications')
    .orderBy('time', 'desc')
    .onSnapshot({
      next: snapshot =>
        watcher({
          type: '@notification/watch:next',
          payload: fromSnapshot(snapshot),
          error: undefined
        }),
      error: err =>
        watcher({
          type: '@notification/watch:error',
          payload: err,
          error: true
        })
    })

  return unsubscribe
}
