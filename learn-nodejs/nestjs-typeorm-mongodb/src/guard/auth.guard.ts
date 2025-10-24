import {
  CanActivate,
  ExecutionContext,
  Injectable,
  mixin,
  Type,
} from '@nestjs/common';
import { Request } from 'express';
import { Observable } from 'rxjs';

export function AuthGuard(
  validateRequest: (
    req: Request,
  ) => boolean | Promise<boolean> | Observable<boolean>,
): Type<CanActivate> {
  @Injectable()
  class Guard implements CanActivate {
    canActivate(context: ExecutionContext): ReturnType<typeof validateRequest> {
      const request = context.switchToHttp().getRequest<Request>();
      return validateRequest(request);
    }
  }

  return mixin(Guard);
}
