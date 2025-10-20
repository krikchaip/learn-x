import {
  Module,
  type MiddlewareConsumer,
  type NestModule,
} from '@nestjs/common';

import { LoggerMiddleware } from 'src/middleware';

import { CatsService } from './cats.service';
import { CatsController } from './cats.controller';

@Module({
  controllers: [CatsController],
  providers: [CatsService],
})
export class CatsModule implements NestModule {
  configure(consumer: MiddlewareConsumer) {
    // apply logger middleware to every path/method in a controller
    consumer.apply(LoggerMiddleware).forRoutes(CatsController);
  }
}
