import firebase from 'services/firebase'

export interface Credentials {
  email: string
  password: string
}

export interface Profile {
  firstname: string
  lastname: string
}

export interface User {
  profile: Profile
  auth: firebase.User
}

export type SignupForm = Credentials & Profile

export const cache = {
  get isLoggedIn() {
    const isLoggedIn = localStorage.getItem('isLoggedIn')
    return isLoggedIn ? JSON.parse(isLoggedIn) : false
  },
  set isLoggedIn(value: boolean) {
    localStorage.setItem('isLoggedIn', String(value))
  }
}

export async function getUserProfile(user: firebase.User) {
  const snapshot = await firebase
    .firestore()
    .collection('users')
    .doc(user.uid)
    .get()

  return (snapshot.data() || null) as Profile | null
}

export async function getCurrentUser() {
  // `firebase.auth().currentUser` somehow out-of-sync
  // when being called in this function. So instead we use AuthStateObserver.
  return new Promise(
    (
      res: (user: User | null) => void,
      rej: (error: firebase.auth.Error) => void
    ) => {
      firebase.auth().onAuthStateChanged(async user => {
        if (!user) return res(null)

        const profile = await getUserProfile(user)
        if (!profile) return null

        return res({ profile, auth: user })
      }, rej)
    }
  )
}

export async function userSignup(form: SignupForm) {
  const { email, password, ...userData } = form

  // automatically signed-in user when success
  const { user } = await firebase
    .auth()
    .createUserWithEmailAndPassword(email, password)

  if (!user) return null

  await firebase
    .firestore()
    .collection('users')
    .doc(user.uid)
    .set(userData)

  return { profile: userData, auth: user } as User
}

export async function userSignin(creds: Credentials) {
  const { user } = await firebase
    .auth()
    .signInWithEmailAndPassword(creds.email, creds.password)

  if (!user) return null

  const profile = await getUserProfile(user)
  if (!profile) return null

  return { profile, auth: user } as User
}

export async function userSignout() {
  return firebase.auth().signOut()
}
