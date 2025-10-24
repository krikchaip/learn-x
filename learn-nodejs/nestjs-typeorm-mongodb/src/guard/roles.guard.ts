import { CanActivate, ExecutionContext, Injectable } from '@nestjs/common';
import { Reflector } from '@nestjs/core';
import { Request } from 'express';
import { Observable } from 'rxjs';

import { Roles } from 'src/decorator';

@Injectable()
export class RolesGuard implements CanActivate {
  constructor(private readonly reflector: Reflector) {}

  canActivate(
    context: ExecutionContext,
  ): boolean | Promise<boolean> | Observable<boolean> {
    const roles = this.reflector.getAllAndOverride(Roles, [
      context.getHandler(),
      context.getClass(),
    ]);

    if (!roles?.length || roles.includes('public')) {
      return true;
    }

    const request = context.switchToHttp().getRequest<Request>();
    const role = request.header('x-role');

    if (!role) {
      return false;
    }

    return roles.includes(role);
  }
}
