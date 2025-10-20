import {
  Module,
  RequestMethod,
  type MiddlewareConsumer,
  type NestModule,
} from '@nestjs/common';

import { HeaderMiddleware, LoggerMiddleware } from 'src/middleware';

import { CatsService } from './cats.service';
import { CatsController } from './cats.controller';

@Module({
  controllers: [CatsController],
  providers: [CatsService],
})
export class CatsModule implements NestModule {
  configure(consumer: MiddlewareConsumer) {
    // apply logger middleware to every path/method in a controller
    consumer
      .apply(LoggerMiddleware)
      .exclude(
        { path: 'cats', method: RequestMethod.POST },
        { path: 'cats/:id', method: RequestMethod.DELETE },
      )
      .forRoutes(CatsController);

    // target specific path and method
    consumer
      .apply(LoggerMiddleware)
      .forRoutes({ path: 'cats', method: RequestMethod.POST });

    // apply multiple middlewares
    consumer
      .apply(
        LoggerMiddleware,
        HeaderMiddleware({ 'X-Operation-Remark': 'DANGER' }),
      )
      .forRoutes({ path: 'cats/:id', method: RequestMethod.DELETE });
  }
}
