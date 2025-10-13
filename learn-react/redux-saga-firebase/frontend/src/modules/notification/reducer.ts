import { Action } from './actions'
import { NotifyObject } from './model'

export type State = typeof initialState

const initialState = {
  list: [] as NotifyObject[],
  error: null as Error | null
}

export default (state: State = initialState, action: Action) => {
  switch (action.type) {
    case '@notification/list:set':
      return { ...state, list: action.payload }
    case '@notification/watch:error':
      return { ...state, error: action.payload }
    default:
      return state
  }
}
