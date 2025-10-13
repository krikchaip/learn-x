import React from 'react'
import { Switch } from 'react-router-dom'

import { AuthRoute } from 'modules/auth'

import Dashboard from 'views/dashboard'
import Navigation from 'views/navigation'
import Project from 'views/project'
import Signin from 'views/signin'
import Signup from 'views/signup'

export const App = () => {
  return (
    <>
      <Navigation />
      <Switch>
        <AuthRoute noAuth path="/signin" component={Signin} />
        <AuthRoute noAuth path="/signup" component={Signup} />
        <AuthRoute path="/project" redirect="/signin" component={Project} />
        <AuthRoute path="/" redirect="/signin" component={Dashboard} />
      </Switch>
    </>
  )
}

export default App
