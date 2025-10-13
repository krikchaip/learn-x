import pipe from 'ramda/src/pipe'

const getState = (state: AppState) => state.project

export const getProjects = pipe(
  getState,
  state => state.projects
)

export const getProject = (state: AppState, id: string) =>
  getProjects(state).find(p => p.id === id)
