import {
  createParamDecorator,
  ExecutionContext,
  NotFoundException,
} from '@nestjs/common';
import { Request } from 'express';

import { type Cat as CatEntity } from './entities/cat.entity';
import { GetCatFromIdPipe } from './cats.pipe';

export const GetCatId = createParamDecorator(
  (data: string, ctx: ExecutionContext) => {
    const request = ctx.switchToHttp().getRequest<Request>();
    const catId = request.header(data);

    if (!catId) {
      throw new NotFoundException();
    }

    return catId;
  },
);

// it works the same way as pipe
export const Cat = (prop?: keyof CatEntity) =>
  GetCatId('X-Cat-Id', GetCatFromIdPipe(prop));
