import { CommonService } from 'src/common';

import { HttpExceptionFilter } from './http-exception.filter';

describe('HttpExceptionFilter', () => {
  it('should be defined', () => {
    expect(new HttpExceptionFilter(new CommonService())).toBeDefined();
  });
});
