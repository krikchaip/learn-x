import z from 'zod';

export const createCatSchema = z.object({
  name: z.string().nonempty(),
  age: z.number().positive(),
  breed: z.string().nonempty(),
});

export type CreateCatDto = z.infer<typeof createCatSchema>;
