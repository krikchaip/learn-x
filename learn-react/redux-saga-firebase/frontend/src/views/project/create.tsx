import React from 'react'
import { RouteComponentProps } from 'react-router-dom'
import { connect } from 'react-redux'

import useFormState, { serialize } from 'lib/hooks/useFormState'

import project from 'modules/project'

type Props = RouteComponentProps & typeof mapDispatchToProps

const mapDispatchToProps = {
  onSubmit: project.actions.create
}

export const Create = (props: Props) => {
  const { onSubmit, history } = props
  const formState = useFormState({ title: '', content: '' })

  function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault()
    onSubmit(serialize(formState))
    history.push('/')
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
        <h5 className="grey-text text-darken-3">Create new project</h5>
        <div className="input-field">
          <label htmlFor="title">Title</label>
          <input type="text" id="title" onChange={handleChange} />
        </div>
        <div className="input-field">
          <label htmlFor="content">Project Content</label>
          <textarea
            id="content"
            className="materialize-textarea"
            onChange={handleChange}
          />
        </div>
        <div className="input-field">
          <button className="btn pink lighten-1 z-depth-0">Create</button>
        </div>
      </form>
    </div>
  )
}

export default connect(
  null,
  mapDispatchToProps
)(Create)
