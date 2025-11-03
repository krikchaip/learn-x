import {
  Module,
  ValidationPipe,
  type MiddlewareConsumer,
  type NestModule,
} from '@nestjs/common';
import { APP_FILTER, APP_GUARD, APP_INTERCEPTOR, APP_PIPE } from '@nestjs/core';
import { TypeOrmModule } from '@nestjs/typeorm';

import { DatabaseModule } from 'src/database';
import { CommonModule } from 'src/common';
import { ConfigModule, DefaultOptionsFactory } from 'src/config';
import { CatsModule, Cat } from 'src/cats';
import { HeaderMiddleware, LoggerMiddleware } from 'src/middleware';
import { AuthGuard } from 'src/guard';
import { TimeoutInterceptor } from 'src/interceptor';
import { UsersModule } from 'src/users';
import { CatchEverythingFilter } from 'src/filter';

import { AppController } from './app.controller';
import { AppService } from './app.service';

@Module({
  imports: [
    // global modules
    CommonModule, // statically registered
    DatabaseModule.forRoot({ entities: [Cat] }), // dynamically registered

    // ConfigModule.forRoot({ folder: './src/config' }),
    // ConfigModule.forRootAsync({
    //   useFactory: () => ({ folder: './src/config' }),
    // }),

    // dynamic modules can also be configured asynchronously using a factory class to provide options.
    ConfigModule.forRootAsync({ useClass: DefaultOptionsFactory }),

    // typeorm integration (mongodb)
    TypeOrmModule.forRoot({
      type: 'mongodb',
      host: 'localhost',
      port: 27017,
      username: 'admin',
      password: 'password',
      database: 'nestjs-typeorm-mongodb',
      authSource: 'admin',
      entities: ['src/**/entities/*.ts'],
    }),

    CatsModule,
    UsersModule,
  ],
  controllers: [AppController],
  providers: [
    AppService,

    // register a global-scoped class-based filter (for DI purpose)
    { provide: APP_FILTER, useClass: CatchEverythingFilter },

    // register a global-scoped guard (for DI purpose)
    { provide: APP_GUARD, useClass: AuthGuard(() => true) },

    // register a global-scoped interceptor (for DI purpose)
    { provide: APP_INTERCEPTOR, useClass: TimeoutInterceptor(1000) },

    // register a global-scoped pipe (for DI purpose)
    {
      provide: APP_PIPE,
      useValue: new ValidationPipe({
        transform: true, // enables transformation of plain objects to DTO instances
        transformOptions: { enableImplicitConversion: true }, // enables automatic type conversion
        validateCustomDecorators: true, // enables validation for decorators other than `Body`, `Param`, etc.
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
