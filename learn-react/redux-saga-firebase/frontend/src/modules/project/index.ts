import * as actions from './actions'
import * as model from './model'
import * as selectors from './selectors'
import reducer from './reducer'
import saga from './saga'

declare module 'modules/project' {
  export type Project = model.Project
  export type Author = model.Author
}

// TODO: module testing
export default {
  actions,
  model,
  selectors,
  reducer,
  saga
}
