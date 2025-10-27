import { Injectable } from '@nestjs/common';

import { CommonService } from 'src/common';

import { ConfigModuleConfig } from '../config.module-definition';

@Injectable()
export class DefaultOptionsFactory {
  constructor(private readonly commonService: CommonService) {}

  create(): ConfigModuleConfig {
    const now = this.commonService.getNowString();
    console.log(`${now} Configuring ConfigModule with DefaultOptions`);

    return {
      folder: './src/config',
    };
  }
}
