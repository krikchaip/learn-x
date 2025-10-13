import { Service } from 'typedi'

export const Token = Reflect.decorate(
  [Service()] as ClassDecorator[],
  jest.fn(() => ({
    sign: jest.fn(() => 'SAMPLE_TOKEN')
  }))
)
