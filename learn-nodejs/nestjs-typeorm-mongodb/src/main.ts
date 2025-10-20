import { NestFactory } from '@nestjs/core';
import { NestExpressApplication } from '@nestjs/platform-express';

import { AppModule } from './app';
import { HeaderMiddleware } from './middleware';

async function bootstrap() {
  const app = await NestFactory.create<NestExpressApplication>(AppModule);

  // apply global middleware
  app.use(HeaderMiddleware({ 'X-Powered-By': 'NestJS' }));

  await app.listen(process.env.PORT ?? 3000);
}

void bootstrap();
