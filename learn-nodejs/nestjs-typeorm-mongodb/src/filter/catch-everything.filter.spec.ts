import { HttpAdapterHost } from '@nestjs/core';

import { CommonService } from 'src/common';

import { CatchEverythingFilter } from './catch-everything.filter';

describe('CatchEverythingFilter', () => {
  it('should be defined', () => {
    expect(
      new CatchEverythingFilter(new HttpAdapterHost(), new CommonService()),
    ).toBeDefined();
  });
});
