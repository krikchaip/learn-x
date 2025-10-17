import { Module } from '@nestjs/common';

import { CommonModule } from 'src/common';
import { CatsModule } from 'src/cats';

import { AppController } from './app.controller';
import { AppService } from './app.service';

@Module({
  imports: [CommonModule, CatsModule],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
