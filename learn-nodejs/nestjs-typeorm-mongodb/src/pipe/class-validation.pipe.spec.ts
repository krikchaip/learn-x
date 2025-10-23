import { CommonService } from 'src/common';

import { ClassValidationPipe } from './class-validation.pipe';

describe('ClassValidationPipe', () => {
  it('should be defined', () => {
    expect(new ClassValidationPipe(new CommonService())).toBeDefined();
  });
});
