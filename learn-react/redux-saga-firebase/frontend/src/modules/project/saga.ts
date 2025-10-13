import { takeLatest, call, put, select } from 'redux-saga/effects'

import auth, { User } from 'modules/auth'

import { CreateAction, save, init } from './actions'
import { fetchProjects, addProject, Project } from './model'

function* initSaga() {
  try {
    const projects: Project[] = yield call(fetchProjects)
    yield put(init(projects))
  } catch {
    console.error('@project/init:error')
  }
}

function* saveSaga(action: CreateAction) {
  try {
    const user: User = yield select(auth.selectors.getUser)
    const project: Project = yield call(addProject, action.payload, {
      ...user.profile,
      id: user.auth.uid
    })

    yield put(save.success(project))
  } catch {
    yield put(save.error(new Error('@project/save:error')))
  }
}

export default function*() {
  yield call(initSaga)
  yield takeLatest('@project/create', saveSaga)
}
