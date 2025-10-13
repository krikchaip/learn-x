import {
  ValidatorConstraint,
  ValidatorConstraintInterface,
  ValidationArguments,
  Validate,
  ValidationOptions
} from 'class-validator'
import { BaseEntity } from 'typeorm'

@ValidatorConstraint({ name: 'notExistsIn', async: true })
class NotExistsInConstraint implements ValidatorConstraintInterface {
  async validate(value: any, args: ValidationArguments) {
    const Entity: typeof BaseEntity = args.constraints[0]
    return !(await Entity.findOne({ [args.property]: value }))
  }

  defaultMessage(args: ValidationArguments) {
    return `$property already exists!`
  }
}

export const NotExistsIn = (
  Entity: typeof BaseEntity,
  options?: ValidationOptions
) => Validate(NotExistsInConstraint, [Entity], options)
