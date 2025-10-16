import { Injectable } from '@nestjs/common';

import { CreateCatDto } from './dto/create-cat.dto';
import { UpdateCatDto } from './dto/update-cat.dto';
import { Cat } from './entities/cat.entity';

@Injectable()
export class CatsService {
  private cats: Cat[] = [];

  private genId(): number {
    const id = this.cats.length + 1;

    const findId = (target: number) => {
      if (this.cats.find((cat) => cat.id === target)) {
        return findId(target + 1);
      } else {
        return target;
      }
    };

    return findId(id);
  }

  create(createCatDto: CreateCatDto) {
    this.cats.push({ ...createCatDto, id: this.genId() });
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
