import React from 'react'
import { connect } from 'react-redux'
import { NavLink, NavLinkProps } from 'react-router-dom'

import omit from 'ramda/src/omit'

import { getIsLoggedIn } from '../selectors'

type Props = ReturnType<typeof mapStateToProps> & NavLinkProps & OwnProps
type OwnProps = {
  noAuth?: boolean
}

const mapStateToProps = (state: AppState) => ({
  isLoggedIn: getIsLoggedIn(state)
})

// TODO: module testing
export const AuthLink = (props: Props) => {
  const { isLoggedIn, noAuth, ...navLinkProps } = props
  const otherProps = omit(['dispatch'], navLinkProps)

  if (noAuth) {
    return isLoggedIn ? null : <NavLink {...otherProps} />
  }

  return isLoggedIn ? <NavLink {...otherProps} /> : null
}

export default connect(mapStateToProps)(AuthLink)
