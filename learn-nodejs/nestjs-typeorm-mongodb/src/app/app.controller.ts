import { Controller, Get, Ip, Query, Req, Res } from '@nestjs/common';
import type { Request, Response } from 'express';

import { AppService } from './app.service';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get()
  getHello(
    @Req() request?: Request,
    @Ip() ip?: string,
    @Query() query?: Record<string, any>,
    @Res({ passthrough: true }) response?: Response,
  ): string {
    console.log(`request url: ${request?.url}`);
    console.log(`request ip: ${ip}`);
    console.log(`request query: ${JSON.stringify(query)}`);

    response?.cookie('nestjs-controller', this.constructor.name);

    return this.appService.getHello();
  }
}
