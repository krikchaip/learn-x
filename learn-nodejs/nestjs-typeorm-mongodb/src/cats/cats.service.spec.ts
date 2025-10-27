import { Test, TestingModule } from '@nestjs/testing';

import { DatabaseModule } from 'src/database';
import { CommonModule } from 'src/common';

import { CatsService } from './cats.service';
import { Cat } from './entities';

describe('CatsService', () => {
  let service: CatsService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      imports: [DatabaseModule.forRoot({ entities: [Cat] }), CommonModule],
      providers: [CatsService],
    }).compile();

    service = module.get<CatsService>(CatsService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
