import { createStore, applyMiddleware } from 'redux'
import { composeWithDevTools } from 'redux-devtools-extension'
import createSagaMiddleware from 'redux-saga'

import reducer from './root-reducer'
import saga from './root-saga'

const sagaMiddleware = createSagaMiddleware()
const middlewares = [sagaMiddleware]

const store = createStore(
  reducer,
  composeWithDevTools(applyMiddleware(...middlewares))
)

sagaMiddleware.run(saga)

export default store
