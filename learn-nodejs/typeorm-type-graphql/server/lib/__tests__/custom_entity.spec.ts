import { Entity, Column, createConnection } from 'typeorm'
import { Contains } from 'class-validator'

import { current } from 'ormconfig'

import { CustomEntity } from 'lib/custom_entity'

it('data shape', async () => {
  @Entity()
  class TestEntity extends CustomEntity {
    @Column()
    field: string
  }

  const { db } = await setup(TestEntity)

  // stubbing class-validator for this test
  const validate = jest
    .spyOn(TestEntity.prototype, 'validate')
    .mockImplementation(async () => undefined)

  const data = {
    field: 'TEST_FIELD',
    id: '98d967c9-be18-4252-b534-de617b0b662f',
    created_at: new Date(),
    updated_at: new Date()
  }

  const result = await TestEntity.create(data).save()

  expect(result).toEqual(data)

  validate.mockRestore()

  return db.close()
})

it('validation', async () => {
  @Entity()
  class TestEntity extends CustomEntity {
    @Column()
    @Contains('hello')
    field: string
  }

  const { db } = await setup(TestEntity)

  const data = { field: 'TEST_FIELD' }

  const result = TestEntity.create(data).save()

  await expect(result).rejects.toMatchInlineSnapshot(
    `[ValidationError: [TestEntity#field] - field must contain a hello string]`
  )

  return db.close()
})

async function setup(entity: typeof CustomEntity) {
  const db = await createConnection({ ...current, entities: [entity] })
  return { db }
}
