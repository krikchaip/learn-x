import {
  Controller,
  Get,
  Header,
  HttpCode,
  HttpStatus,
  Ip,
  Param,
  Query,
  Redirect,
  Req,
  Res,
  UseFilters,
  type HttpRedirectResponse,
} from '@nestjs/common';
import { concatMap, Observable, of, timer } from 'rxjs';
import type { Request, Response } from 'express';

import { HttpExceptionFilter } from 'src/filter';

import { AppService } from './app.service';
import { ForbiddenException } from './app.exception';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get()
  @HttpCode(HttpStatus.NO_CONTENT)
  @Header('x-nestjs-handler', 'getHello')
  getHello(
    @Req() request?: Request,
    @Ip() ip?: string,
    @Query() query?: Record<string, any>,
    @Res({ passthrough: true }) response?: Response,
  ): string {
    console.log(`request url: ${request?.url}`);
    console.log(`request ip: ${ip}`);
    console.log(`request query: ${JSON.stringify(query)}`);

    // Set HTTP Cookie
    response?.cookie('nestjs-controller', this.constructor.name);

    return this.appService.getHello();
  }

  // @Get('wildcard/*splat') // mark `splat` as REQUIRED
  @Get('wildcard/{*splat}') // mark `splat` as OPTIONAL
  @UseFilters(HttpExceptionFilter)
  wildcard(@Param('splat') splat: string[]) {
    // splat:
    // - '/wildcard/123' -> ['123']
    // - '/wildcard/123/winner' -> ['123', 'winner']

    // custom exception that extends HttpException
    if (splat[0] === 'forbidden') {
      throw new ForbiddenException();
    }

    return JSON.stringify(splat);
  }

  @Get('docs')
  @Redirect('https://docs.nestjs.com', HttpStatus.FOUND)
  getDocs(
    @Query('version') version?: string,
  ): HttpRedirectResponse | undefined {
    if (version === '5') {
      // Returned values will override any arguments passed to the @Redirect() decorator
      return {
        url: 'https://docs.nestjs.com/v5/',
        statusCode: HttpStatus.FOUND,
      };
    }
  }

  @Get('error')
  error() {
    throw new Error('throwing generic error');
  }

  @Get('slow')
  slow(@Query('delay') delay: number = 1000): Observable<string> {
    // a method handler can also return rxjs's observable
    return timer(delay).pipe(concatMap(() => of("I'm so slow, baby.")));
  }
}
