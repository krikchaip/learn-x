import {
  CallHandler,
  ExecutionContext,
  Injectable,
  mixin,
  NestInterceptor,
  RequestTimeoutException,
  Type,
} from '@nestjs/common';
import {
  catchError,
  Observable,
  throwError,
  timeout,
  TimeoutError,
} from 'rxjs';

export function TimeoutInterceptor(ms: number = 10000): Type<NestInterceptor> {
  @Injectable()
  class Interceptor implements NestInterceptor {
    intercept(_: ExecutionContext, next: CallHandler): Observable<any> {
      return next.handle().pipe(
        timeout(ms),
        catchError((err: Error) => {
          if (err instanceof TimeoutError)
            return throwError(() => new RequestTimeoutException());
          else return throwError(() => err);
        }),
      );
    }
  }

  return mixin(Interceptor);
}
