import { NotifyObject } from './model'

export type Action = ListAction | WatchAction

export type ListAction = ReturnType<typeof list.set>
export const list = {
  set: (list: NotifyObject[]) => ({
    type: '@notification/list:set' as const,
    payload: list
  })
}

export type WatchAction<T extends object = object> =
  | { type: '@notification/watch:next'; payload: T; error: undefined }
  | { type: '@notification/watch:error'; payload: Error; error: true }
