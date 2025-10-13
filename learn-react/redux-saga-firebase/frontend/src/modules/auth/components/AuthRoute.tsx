import React from 'react'
import { connect } from 'react-redux'
import { Route, Redirect, RouteProps } from 'react-router-dom'

import { getIsLoggedIn } from '../selectors'

type Props = ReturnType<typeof mapStateToProps> & RouteProps & OwnProps
type OwnProps = {
  redirect?: string
  noAuth?: boolean
}

const mapStateToProps = (state: AppState) => ({
  isLoggedIn: getIsLoggedIn(state)
})

// TODO: module testing
export const AuthRoute = (props: Props) => {
  const { isLoggedIn, redirect = '/', noAuth, ...routeProps } = props

  if (noAuth) {
    return isLoggedIn ? <Redirect to={redirect} /> : <Route {...routeProps} />
  }

  return isLoggedIn ? <Route {...routeProps} /> : <Redirect to={redirect} />
}

export default connect(mapStateToProps)(AuthRoute)
