import {
  Module,
  type MiddlewareConsumer,
  type NestModule,
} from '@nestjs/common';
import { APP_FILTER } from '@nestjs/core';

import { DatabaseModule } from 'src/database';
import { CommonModule } from 'src/common';
import { CatsModule } from 'src/cats';
import { HeaderMiddleware, LoggerMiddleware } from 'src/middleware';

import { AppController } from './app.controller';
import { AppService } from './app.service';
import { CatchEverythingFilter } from 'src/filter';

@Module({
  imports: [
    // global modules
    CommonModule,
    DatabaseModule.forRoot(), // dynamically registered

    CatsModule,
  ],
  controllers: [AppController],
  providers: [
    AppService,

    // register global-scoped class-based filters (for DI purpose)
    { provide: APP_FILTER, useClass: CatchEverythingFilter },
  ],
})
export class AppModule implements NestModule {
  configure(consumer: MiddlewareConsumer) {
    // apply a middleware to wildcard routes
    // consumer.apply(LoggerMiddleware).forRoutes('wildcard/*splat');
    consumer.apply(LoggerMiddleware).forRoutes('wildcard/{*splat}');

    // apply global middleware (for DI purpose)
    consumer
      .apply(HeaderMiddleware({ 'X-Operation-Remark': 'NORMAL' }))
      .forRoutes('*');
  }
}
