import { validate } from 'class-validator'

import { NotExistsIn } from '../not-exists-in'

it('default message', async () => {
  const { Entity } = setup()

  const entity = new Entity()
  entity.SOME_FIELD = 'EXISTS'

  const errors = await validate(entity)

  expect(errors).toMatchInlineSnapshot(`
    Array [
      ValidationError {
        "children": Array [],
        "constraints": Object {
          "notExistsIn": "SOME_FIELD already exists!",
        },
        "property": "SOME_FIELD",
        "target": Entity {
          "SOME_FIELD": "EXISTS",
        },
        "value": "EXISTS",
      },
    ]
  `)

  expect(Entity.findOne).toBeCalledTimes(1)
  expect(Entity.findOne.mock.calls[0]).toMatchInlineSnapshot(`
    Array [
      Object {
        "SOME_FIELD": "EXISTS",
      },
    ]
  `)
})

function setup() {
  class Entity {
    @NotExistsIn(Entity as any)
    SOME_FIELD: string

    static findOne = jest.fn().mockResolvedValue(true)
  }

  return { Entity }
}
