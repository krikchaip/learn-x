import React from 'react'
import { connect } from 'react-redux'
import { Link } from 'react-router-dom'
import moment from 'moment'

import project from 'modules/project'

type Props = ReturnType<typeof mapStateToProps>

const mapStateToProps = (state: AppState) => ({
  projects: project.selectors.getProjects(state)
})

export const Projects = (props: Props) => {
  const { projects } = props
  return (
    <div className="project-list section">
      {projects.length > 0 ? (
        projects.map(p => (
          <Link to={`/project/${p.id}`} key={p.id}>
            <div className="card z-depth-0 project-summary">
              <div className="card-content grey-text text-darken-3">
                <span className="card-title ">{p.title}</span>
                <p>{p.content}</p>
                <p className="grey-text">{moment(p.createdAt).calendar()}</p>
              </div>
            </div>
          </Link>
        ))
      ) : (
        <div className="container left">Loading Projects...</div>
      )}
    </div>
  )
}

export default connect(mapStateToProps)(Projects)
