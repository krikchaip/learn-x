import { SetMetadata } from '@nestjs/common';
import { Reflector } from '@nestjs/core';

// with `@SetMetadata` decorator
export const RolesDecorator = (...roles: string[]) =>
  SetMetadata('Roles', roles);

// with `Reflector` utility
export const RolesReflector = Reflector.createDecorator<string[]>();
