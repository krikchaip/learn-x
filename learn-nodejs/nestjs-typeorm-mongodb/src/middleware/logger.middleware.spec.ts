import { CommonService } from 'src/common';

import { LoggerMiddleware } from './logger.middleware';

describe('LoggerMiddleware', () => {
  it('should be defined', () => {
    expect(new LoggerMiddleware(new CommonService())).toBeDefined();
  });
});
