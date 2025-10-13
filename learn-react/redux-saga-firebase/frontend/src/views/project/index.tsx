import React from 'react'
import { Switch, Route, RouteComponentProps } from 'react-router-dom'

import Create from './create'
import Details from './details'

type Props = RouteComponentProps

// TODO: module testing
export const Project = (props: Props) => {
  const { match } = props
  return (
    <Switch>
      <Route path={`${match.path}/create`} component={Create} />
      <Route path={`${match.path}/:id`} component={Details} />
    </Switch>
  )
}

export default Project
