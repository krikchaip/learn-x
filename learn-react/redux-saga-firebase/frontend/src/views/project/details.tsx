import React from 'react'
import { connect } from 'react-redux'
import { RouteComponentProps } from 'react-router-dom'
import moment from 'moment'

import project from 'modules/project'

type Props = ReturnType<typeof mapStateToProps> & OwnProps
type OwnProps = RouteComponentProps<{ id: string }>

const mapStateToProps = (state: AppState, props: OwnProps) => ({
  project: project.selectors.getProject(state, props.match.params.id)
})

export const Details = (props: Props) => {
  const { project } = props

  if (!project) {
    return (
      <div className="container center">
        <p>Loading Project...</p>
      </div>
    )
  }

  return (
    <div className="container section project-details">
      <div className="card z-depth-0">
        <div className="card-content">
          <span className="card-title">{project.title}</span>
          <p>{project.content}</p>
        </div>
        <div className="card-action grey lighten-4 grey-text">
          <div>
            Posted by {project.authorFirstName} {project.authorLastName}
          </div>
          <div>{moment(project.createdAt).calendar()}</div>
        </div>
      </div>
    </div>
  )
}

export default connect(mapStateToProps)(Details)
