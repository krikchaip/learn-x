import { Injectable, NestMiddleware } from '@nestjs/common';
import type { NextFunction, Request, Response } from 'express';

import { CommonService } from 'src/common';

@Injectable()
export class LoggerMiddleware implements NestMiddleware {
  constructor(private readonly commonService: CommonService) {}

  use(req: Request, _: Response, next: NextFunction) {
    const timestamp = this.commonService.getNowString();
    const method = req.method;
    const url = req.url;

    console.log(`${timestamp} [${method}] ${url}`);

    next();
  }
}
