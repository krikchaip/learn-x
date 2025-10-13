import { Project } from './model'

export type Action = InitAction | CreateAction | SaveAction

export type InitAction = ReturnType<typeof init>
export const init = (projects: Project[]) => ({
  type: '@project/init' as const,
  payload: projects
})

export type CreateAction = ReturnType<typeof create>
export const create = (project: Pick<Project, 'title' | 'content'>) => ({
  type: '@project/create' as const,
  payload: project
})

export type SaveAction =
  | ReturnType<typeof save.success>
  | ReturnType<typeof save.error>
export const save = {
  success: (project: Project) => ({
    type: '@project/save:success' as const,
    payload: project
  }),
  error: (err: Error) => ({
    type: '@project/save:error' as const,
    payload: err,
    error: true as const
  })
}
