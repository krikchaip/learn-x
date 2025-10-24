import {
  Module,
  ValidationPipe,
  type MiddlewareConsumer,
  type NestModule,
} from '@nestjs/common';
import { APP_FILTER, APP_GUARD, APP_PIPE } from '@nestjs/core';

import { DatabaseModule } from 'src/database';
import { CommonModule } from 'src/common';
import { CatsModule } from 'src/cats';
import { HeaderMiddleware, LoggerMiddleware } from 'src/middleware';

import { AppController } from './app.controller';
import { AppService } from './app.service';
import { CatchEverythingFilter } from 'src/filter';
import { AuthGuard } from 'src/guard';

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

    // register a global-scoped class-based filter (for DI purpose)
    { provide: APP_FILTER, useClass: CatchEverythingFilter },

    // register a global-scoped guard (for DI purpose)
    { provide: APP_GUARD, useClass: AuthGuard(() => true) },

    // register a global-scoped pipe (for DI purpose)
    {
      provide: APP_PIPE,
      useValue: new ValidationPipe({
        transform: true, // enables transformation of plain objects to DTO instances
        transformOptions: { enableImplicitConversion: true }, // enables automatic type conversion
      }),
    },
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
