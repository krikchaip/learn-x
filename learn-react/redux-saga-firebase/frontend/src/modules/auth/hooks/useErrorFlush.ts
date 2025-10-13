import { useEffect } from 'react'
import { Dispatch } from 'redux'
import { useDispatch, useSelector } from 'react-redux'

import { Action, flush } from '../actions'
import { getError } from '../selectors'

// TODO: module testing
export default () => {
  const dispatch = useDispatch<Dispatch<Action>>()
  const error = useSelector(getError)

  useEffect(() => {
    if (error) {
      dispatch(flush.error())
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])
}
