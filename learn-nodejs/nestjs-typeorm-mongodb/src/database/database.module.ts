import { type DynamicModule, Module, Type } from '@nestjs/common';

import { DatabaseService } from './database.service';

interface DatabaseModuleConfig {
  entities: NonEmptyArray<Type>;
}

@Module({
  providers: [DatabaseService],
  exports: [],
})
export class DatabaseModule {
  static forRoot(options: DatabaseModuleConfig): DynamicModule {
    return {
      // makes this dynamic module 'global', similar to applying `@Global`
      global: true,

      // [required] uses options specified in `@Module` as a base module config
      module: DatabaseModule,

      // extends (not overrides) the 'providers' option specified in `@Module`
      providers: [
        {
          provide: 'COLLECTIONS',
          useFactory: () => {
            const { entities } = options;
            const collections: Record<string, any[]> = {};

            entities.forEach((entity) => {
              collections[entity.name] = [];
            });

            return collections;
          },
        },
      ],

      // extends (not overrides) the 'exports' option specified in `@Module`
      exports: [DatabaseService],
    };
  }
}
