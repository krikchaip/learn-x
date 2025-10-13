import { call, put, fork, take, select, takeLatest } from 'redux-saga/effects'

import { signup, signin, signout, init } from './actions'
import {
  userSignin,
  userSignout,
  getCurrentUser,
  cache,
  userSignup,
  User
} from './model'
import { getUser } from './selectors'

function* initSaga() {
  // pre-loaded cache
  let isLoggedIn: boolean = yield call(() => cache.isLoggedIn)
  yield put(init.isLoggedIn(isLoggedIn))

  const user: User | null = yield call(getCurrentUser)
  isLoggedIn = yield call(() => (cache.isLoggedIn = !!user))

  yield put(init.user(user))
  yield put(init.isLoggedIn(isLoggedIn))
}

function* signupSaga(action: ReturnType<typeof signup.attempt>) {
  try {
    const user: User | null = yield call(userSignup, action.payload)
    const isLoggedIn: boolean = yield call(() => (cache.isLoggedIn = !!user))

    yield put(signup.success(user, isLoggedIn))
  } catch (firebaseError) {
    const err = new Error(firebaseError.message)
    err.name = firebaseError.code

    yield put(signup.error(err))
  }
}

function* signinSaga(action: ReturnType<typeof signin.attempt>) {
  try {
    const user: User | null = yield call(userSignin, action.payload)

    // logged-in state caching
    const isLoggedIn: boolean = yield call(() => (cache.isLoggedIn = !!user))

    return yield put(signin.success(user, isLoggedIn))
  } catch {
    return yield put(signin.error(new Error('@auth/signin:error')))
  }
}

// try logging out until success
function* signoutSaga() {
  while (true) {
    try {
      yield call(userSignout)
    } catch {
      continue
    }

    // logged-out state caching
    yield call(() => (cache.isLoggedIn = false))

    yield put(signout.success())
    break
  }
}

function* userAuthSaga() {
  while (true) {
    const user: ReturnType<typeof getUser> = yield select(getUser)
    if (!user) {
      const attempt = yield take('@auth/signin:attempt')
      const { error } = yield call(signinSaga, attempt)

      // re-login when an error occured
      if (error) continue
    }

    yield take('@auth/signout:attempt')
    yield call(signoutSaga)
  }
}

export default function*() {
  yield call(initSaga)
  yield takeLatest('@auth/signup:attempt', signupSaga)
  yield fork(userAuthSaga)
}
