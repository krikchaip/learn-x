import {
  ArgumentsHost,
  Catch,
  ExceptionFilter,
  HttpException,
} from '@nestjs/common';
import type { Request, Response } from 'express';

import { CommonService } from 'src/common';

@Catch(HttpException)
export class HttpExceptionFilter implements ExceptionFilter {
  constructor(private readonly commonService: CommonService) {}

  catch(exception: HttpException, host: ArgumentsHost) {
    const status = exception.getStatus();
    const body = exception.getResponse();
    const ctx = host.switchToHttp();

    const request = ctx.getRequest<Request>();
    const response = ctx.getResponse<Response>();

    response.status(status).json({
      path: request.url,
      timestamp: this.commonService.getNowString(),
      ...(typeof body === 'object'
        ? body
        : { message: body, statusCode: status }),
    });
  }
}
