import reducer from 'modules/root-reducer'

/**
 * Declaring a global type (with top-level import/export)
 */
declare global {
  type AppState = ReturnType<typeof reducer>
}
