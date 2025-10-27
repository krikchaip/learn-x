import { Global, Module } from '@nestjs/common';

import { CommonService } from './common.service';

@Global() // makes this module 'Global', i.e., no need to import it through `imports` property
@Module({
  providers: [CommonService],
  exports: [CommonService],
})
export class CommonModule {}
