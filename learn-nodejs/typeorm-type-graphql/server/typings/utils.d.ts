type ArrayElement<T> = T extends (infer U)[] ? U : never

type Awaited<T> = T extends PromiseLike<infer U> ? Awaited<U> : T

/**
 * Overriding object property types
 * @see https://stackoverflow.com/questions/41285211/overriding-interface-property-type-defined-in-typescript-d-ts-file
 */
type Patch<T, R> = Omit<T, keyof R> & R

type Tuple<T, U> = [T, U]
