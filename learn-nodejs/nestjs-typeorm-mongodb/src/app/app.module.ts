import {
  Module,
  type MiddlewareConsumer,
  type NestModule,
} from '@nestjs/common';

import { DatabaseModule } from 'src/database';
import { CommonModule } from 'src/common';
import { CatsModule } from 'src/cats';
import { LoggerMiddleware } from 'src/middleware';

import { AppController } from './app.controller';
import { AppService } from './app.service';

@Module({
  imports: [DatabaseModule.forRoot(), CommonModule, CatsModule],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule implements NestModule {
  configure(consumer: MiddlewareConsumer) {
    // apply a middleware to wildcard routes
    // consumer.apply(LoggerMiddleware).forRoutes('wildcard/*splat');
    consumer.apply(LoggerMiddleware).forRoutes('wildcard/{*splat}');
  }
}
