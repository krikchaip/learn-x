import { Injectable } from '@nestjs/common';

import { CommonService } from 'src/common';

import { CreateCatDto } from './dto/create-cat.dto';
import { UpdateCatDto } from './dto/update-cat.dto';
import { Cat } from './entities/cat.entity';

@Injectable()
export class CatsService {
  private cats: Cat[] = [];

  constructor(private readonly commonService: CommonService) {}

  create(createCatDto: CreateCatDto) {
    this.cats.push({
      ...createCatDto,
      id: this.commonService.genNextIdx(this.cats, 'id'),
    });
  }

  findAll() {
    return this.cats;
  }

  findOne(id: number) {
    return this.cats.find((cat) => cat.id === id);
  }

  update(id: number, updateCatDto: UpdateCatDto) {
    const idx = this.cats.findIndex((cat) => cat.id === id);

    if (idx === -1) {
      return;
    }

    this.cats[idx] = { ...this.cats[idx], ...updateCatDto };

    return this.cats[idx];
  }

  remove(id: number) {
    const idx = this.cats.findIndex((cat) => cat.id === id);

    if (idx === -1) {
      return;
    }

    const removedCat = this.cats[idx];
    this.cats.splice(idx, 1);

    return removedCat;
  }
}
