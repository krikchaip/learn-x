import {
  applyDecorators,
  createParamDecorator,
  ExecutionContext,
  NotFoundException,
  UseFilters,
  UseGuards,
  UseInterceptors,
} from '@nestjs/common';
import { Request } from 'express';

import { Roles } from 'src/decorator';
import { HttpExceptionFilter } from 'src/filter';
import { RolesGuard } from 'src/guard';
import { MapResponseInterceptor } from 'src/interceptor';

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

// works the same way as other builtin param decorators (`@Body`, `@Query`, ...)
export const Cat = (prop?: keyof CatEntity) =>
  GetCatId('X-Cat-Id', GetCatFromIdPipe(prop));

export function CatsControllerOpts(role: string) {
  return applyDecorators(
    UseFilters(HttpExceptionFilter),
    UseGuards(RolesGuard),
    UseInterceptors(MapResponseInterceptor),

    // applying target metadata (use Reflector module to get it)
    Roles([role]),
  );
}
