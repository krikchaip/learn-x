import { getRandomAnimal } from '@nx-typescript-lambda/animal';
import { formatMessage } from '@nx-typescript-lambda/util';

export function zoo(): string {
  const animal = getRandomAnimal();
  const message = `${animal.name} says ${animal.sound}`;

  return formatMessage('ZOO', message);
}
