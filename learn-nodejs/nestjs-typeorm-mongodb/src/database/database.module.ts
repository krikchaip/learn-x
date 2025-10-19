import { type DynamicModule, Global, Module } from '@nestjs/common';

import { DatabaseService } from './database.service';

@Global()
@Module({
  providers: [DatabaseService],
  exports: [DatabaseService],
})
export class DatabaseModule {
  static forRoot(): DynamicModule {
    return {
      global: true,
      module: DatabaseModule,
      providers: [],
      exports: [],
    };
  }
}
