import { Module } from '@nestjs/common';

import { ConfigurableModuleClass } from './config.module-definition';
import { ConfigService } from './config.service';

@Module({
  providers: [ConfigService],
  exports: [ConfigService],
})
export class ConfigModule extends ConfigurableModuleClass {
  // moved the module option creation logic to `config.module-definition` file instead
  // static forRoot(options?: ConfigModuleConfig): DynamicModule {
  //   return {
  //     // makes this dynamic module 'global', similar to applying `@Global`
  //     global: true,
  //
  //     // [required] uses options specified in `@Module` as a base module config
  //     module: ConfigModule,
  //
  //     // extends (not overrides) the 'providers' option specified in `@Module`
  //     providers: [
  //       ConfigService,
  //       { provide: 'CONFIG_OPTIONS', useValue: options },
  //     ],
  //
  //     // extends (not overrides) the 'exports' option specified in `@Module`
  //     exports: [ConfigService],
  //   };
  // }
}
