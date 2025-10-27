import { Injectable } from '@nestjs/common';
import fs from 'fs';
import path from 'path';
import dotenv from 'dotenv';

import { EnvConfig } from './entities';

@Injectable()
export class ConfigService {
  private envConfig: EnvConfig;

  constructor() {
    const options = { folder: './config' };

    const { NODE_ENV } = process.env;
    const filePath = `${NODE_ENV ? `.${NODE_ENV}` : ''}.env`;
    const envFile = path.resolve(__dirname, '../../', options.folder, filePath);

    try {
      this.envConfig = dotenv.parse(fs.readFileSync(envFile));
    } catch {
      this.envConfig = {};
    }
  }

  get<K extends keyof EnvConfig>(key: K): EnvConfig[K] {
    return this.envConfig[key];
  }
}
