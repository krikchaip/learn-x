import {
  CallHandler,
  ExecutionContext,
  Injectable,
  NestInterceptor,
} from '@nestjs/common';
import { Observable, tap } from 'rxjs';

import { CommonService } from 'src/common';

@Injectable()
export class LoggingInterceptor implements NestInterceptor {
  constructor(private readonly commonService: CommonService) {}

  intercept(_: ExecutionContext, next: CallHandler): Observable<any> {
    console.log(`${this.commonService.getNowString()} Before logging...`);

    const start = Date.now();
    const stream$ = next.handle().pipe(
      tap(() => {
        const elapsed = Date.now() - start;
        const nowString = this.commonService.getNowString();

        console.log(`${nowString} After logging... ${elapsed}ms`);
      }),
    );

    return stream$;
  }
}
