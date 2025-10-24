import {
  CallHandler,
  ExecutionContext,
  Injectable,
  NestInterceptor,
} from '@nestjs/common';
import { map, Observable } from 'rxjs';

import { CommonService } from 'src/common';

export interface Response<T> {
  data: T;
  timestamp: string;
  handler: string;
}

@Injectable()
export class MapResponseInterceptor<T>
  implements NestInterceptor<T, Response<T>>
{
  constructor(private readonly commonService: CommonService) {}

  intercept(
    context: ExecutionContext,
    next: CallHandler<T>,
  ): Observable<Response<T>> {
    const handler = context.getHandler().name;

    return next.handle().pipe(
      map((data) => ({
        data,
        handler,
        timestamp: this.commonService.getNowString(),
      })),
    );
  }
}
