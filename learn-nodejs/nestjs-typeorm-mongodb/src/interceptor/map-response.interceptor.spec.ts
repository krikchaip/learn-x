import { CommonService } from 'src/common';

import { MapResponseInterceptor } from './map-response.interceptor';

describe('MapResponseInterceptor', () => {
  it('should be defined', () => {
    expect(new MapResponseInterceptor(new CommonService())).toBeDefined();
  });
});
