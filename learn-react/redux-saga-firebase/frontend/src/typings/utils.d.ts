/**
 * Unpacking type `T` from `T[]`
 */
declare type Unpack<T> = T extends (infer U)[] ? U : T

/**
 * Making sure that type parameter `T` is a *Plain Old Javascript Object*
 */
declare type POJO<T> = T extends object
  ? (keyof T extends string ? T : never)
  : never
