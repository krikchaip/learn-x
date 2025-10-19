import { Injectable } from '@nestjs/common';

import { DatabaseService } from 'src/database';

import { CreateCatDto } from './dto/create-cat.dto';
import { UpdateCatDto } from './dto/update-cat.dto';
import { Cat } from './entities/cat.entity';

@Injectable()
export class CatsService {
  constructor(private readonly db: DatabaseService) {}

  create(createCatDto: CreateCatDto) {
    this.db.insert(Cat, createCatDto as Cat);
  }

  findAll() {
    return this.db.select(Cat);
  }

  findOne(id: number): Cat | undefined {
    return this.db.select(Cat, [['id', id]])[0];
  }

  update(id: number, updateCatDto: UpdateCatDto): Cat | undefined {
    return this.db.update(Cat, [['id', id]], updateCatDto as Cat)[0];
  }

  remove(id: number): Cat | undefined {
    return this.db.delete(Cat, [['id', id]])[0];
  }
}
