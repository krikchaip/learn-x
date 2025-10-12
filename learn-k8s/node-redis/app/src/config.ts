export const PORT = process.env.PORT ? Number(process.env.PORT) : 8080
export const REDIS_URL = process.env.REDIS_URL ?? 'redis://localhost:6379'
