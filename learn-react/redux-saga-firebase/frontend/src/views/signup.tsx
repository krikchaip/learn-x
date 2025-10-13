import React from 'react'
import { connect } from 'react-redux'

import useFormState, { serialize } from 'lib/hooks/useFormState'

import auth, { useErrorFlush } from 'modules/auth'

type Props = ReturnType<typeof mapStateToProps> & typeof mapDispatchToProps

const mapStateToProps = (state: AppState) => ({
  error: auth.selectors.getError(state)
})

const mapDispatchToProps = {
  signup: auth.actions.signup.attempt
}

// TODO: module testing
export const Signup = (props: Props) => {
  const { signup, error } = props
  const formState = useFormState({
    email: '',
    password: '',
    firstname: '',
    lastname: ''
  })

  useErrorFlush()

  function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault()
    signup(serialize(formState))
  }

  /** element target id update pattern */
  function handleChange(
    e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>
  ) {
    formState[e.target.id as keyof (typeof formState)].set(e.target.value)
  }

  return (
    <div className="container">
      <form className="white" onSubmit={handleSubmit}>
        <h5 className="grey-text text-darken-3">Sign Up</h5>
        <div className="input-field">
          <label htmlFor="email">Email</label>
          <input type="email" id="email" onChange={handleChange} />
        </div>
        <div className="input-field">
          <label htmlFor="password">Password</label>
          <input type="password" id="password" onChange={handleChange} />
        </div>
        <div className="input-field">
          <label htmlFor="firstname">Firstname</label>
          <input type="text" id="firstname" onChange={handleChange} />
        </div>
        <div className="input-field">
          <label htmlFor="lastname">Lastname</label>
          <input type="text" id="lastname" onChange={handleChange} />
        </div>
        <div className="input-field">
          <button className="btn pink lighten-1 z-depth-0">SIGN UP</button>
        </div>
        {error && (
          <div className="red-text center">
            <p>{error.message}</p>
          </div>
        )}
      </form>
    </div>
  )
}

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(Signup)
