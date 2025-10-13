import React from 'react'
import { connect } from 'react-redux'

import auth, { AuthLink } from 'modules/auth'

type Props = ReturnType<typeof mapStateToProps> & typeof mapDispatchToProps

const mapStateToProps = (state: AppState) => ({
  initials: auth.selectors.getUserInitials(state)
})

const mapDispatchToProps = {
  signout: auth.actions.signout.attempt
}

export const Menu = (props: Props) => {
  const { signout, initials } = props
  return (
    <ul className="right">
      <li>
        <AuthLink noAuth to="/signup">
          Signup
        </AuthLink>
      </li>
      <li>
        <AuthLink noAuth to="/signin">
          Signin
        </AuthLink>
      </li>
      <li>
        <AuthLink to="/project/create">New Project</AuthLink>
      </li>
      <li>
        <AuthLink to="/" onClick={signout}>
          Log Out
        </AuthLink>
      </li>
      <li>
        <AuthLink to="/" className="btn btn-floating pink lighten-1">
          {initials.toUpperCase()}
        </AuthLink>
      </li>
    </ul>
  )
}

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(Menu)
