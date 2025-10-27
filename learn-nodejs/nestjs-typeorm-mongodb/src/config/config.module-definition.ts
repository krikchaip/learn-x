import { ConfigurableModuleBuilder } from '@nestjs/common';

export interface ConfigModuleConfig {
  folder?: string;
}

export const { ConfigurableModuleClass, MODULE_OPTIONS_TOKEN } =
  new ConfigurableModuleBuilder<ConfigModuleConfig>()
    .setExtras({ global: true }, (definition, { global }) => ({
      ...definition,
      global,
    }))
    .setClassMethodName('forRoot')
    .setFactoryMethodName('create')
    .build();
