import { useReducer, Reducer } from 'react'

type FormState<T> = {
  [K in keyof T]: {
    get: () => T[K]
    set: (x: T[K]) => void
    update: (f: (x: T[K]) => T[K]) => void
  }
}

type FormAction<T> = {
  type: keyof T
  payload: T[keyof T] | ((x: T[keyof T]) => T[keyof T])
}

function createFormReducer<T>(
  initialState: POJO<T>
): Reducer<T, FormAction<T>> {
  return function(state, action) {
    const { type: key, payload } = action

    if (!(key in initialState)) return state
    if (payload instanceof Function)
      return { ...state, [key]: payload(state[key]) }

    return { ...state, [key]: payload }
  }
}

export function serialize<T>(formState: FormState<T>): T {
  let serializedState = {} as T
  for (const field in formState) {
    serializedState[field] = formState[field].get()
  }

  return serializedState
}

export default <T>(initialState: POJO<T>) => {
  const formReducer = createFormReducer(initialState)
  const [state, dispatch] = useReducer(formReducer, initialState)

  let formState = {} as FormState<T>
  for (const field in state) {
    formState[field as keyof T] = {
      get: () => state[field],
      set: value => dispatch({ type: field, payload: value }),
      update: f => dispatch({ type: field, payload: f })
    }
  }

  return formState
}
