import { firestore } from 'firebase-admin'

declare global {
  interface NotifyObject {
    detail: string
    time: firestore.Timestamp
    who: {
      id: string
      name: string
    }
  }
}
