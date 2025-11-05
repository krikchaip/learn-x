import { zoo } from './zoo.js';

describe('zoo', () => {
  it('should work', () => {
    expect(zoo()).toMatch(/\[ZOO\] (\w+) says (\w+)/);
  });
});
