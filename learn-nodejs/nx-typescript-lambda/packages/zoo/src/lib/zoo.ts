import { getRandomAnimal } from '@nx-typescript-lambda/animal';

export function zoo(): string {
  const animal = getRandomAnimal();
  return `${animal.name} says ${animal.sound}`;
}
