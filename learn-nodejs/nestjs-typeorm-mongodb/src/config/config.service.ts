import { Injectable, Inject, Optional } from '@nestjs/common';
import fs from 'fs';
import path from 'path';
import dotenv from 'dotenv';

import { type ConfigModuleConfig } from './config.module';
import { EnvConfig } from './entities';

@Injectable()
export class ConfigService {
  private envConfig: EnvConfig;

  constructor(
    @Inject('CONFIG_OPTIONS')
    @Optional()
    private readonly options?: ConfigModuleConfig,
  ) {
    const { NODE_ENV } = process.env;

    const filePath = `${NODE_ENV ? `.${NODE_ENV}` : ''}.env`;
    const envFile = path.resolve(
      __dirname,
      '../../',
      this.options?.folder ?? '.',
      filePath,
    );

    try {
      this.envConfig = dotenv.parse(fs.readFileSync(envFile));
    } catch {
      this.envConfig = {};
    }
  }

  get<K extends keyof EnvConfig>(key?: K): EnvConfig[K] | EnvConfig {
    return key ? this.envConfig[key] : this.envConfig;
  }
}
