import { type CreateCatDto } from './create-cat.dto';

export type UpdateCatDto = Partial<CreateCatDto>;
