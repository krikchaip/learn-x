import {
  ArgumentsHost,
  Catch,
  ExceptionFilter,
  HttpException,
  HttpStatus,
} from '@nestjs/common';
import { HttpAdapterHost } from '@nestjs/core';

import { CommonService } from 'src/common';

@Catch() // the 'catch all error' filter -> leave the params empty
export class CatchEverythingFilter implements ExceptionFilter {
  constructor(
    private readonly httpAdapterHost: HttpAdapterHost,
    private readonly commonService: CommonService,
  ) {}

  //
  catch(exception: unknown, host: ArgumentsHost) {
    // a generic way to retrieve the express/fastify application instance
    const { httpAdapter } = this.httpAdapterHost;

    const ctx = host.switchToHttp();

    const request = ctx.getRequest(); // eslint-disable-line
    const response = ctx.getResponse(); // eslint-disable-line

    const statusCode =
      exception instanceof HttpException
        ? exception.getStatus()
        : HttpStatus.INTERNAL_SERVER_ERROR;

    const reason = exception instanceof Error ? exception.message : undefined;

    const body = {
      type: 'CATCH_ALL',
      statusCode,
      timestamp: this.commonService.getNowString(),
      path: httpAdapter.getRequestUrl(request), // eslint-disable-line
      reason,
    };

    // an equivalent of res.json() in express, or reply.send() in fastify
    httpAdapter.reply(response, body, statusCode);
  }
}
