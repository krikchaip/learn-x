import {
  BadRequestException,
  Injectable,
  mixin,
  NotFoundException,
  PipeTransform,
} from '@nestjs/common';

import { Cat as CatEntity } from './entities/cat.entity';
import { CatsService } from './cats.service';

export function GetCatFromIdPipe(prop?: keyof CatEntity) {
  @Injectable()
  class GetCatFromIdPipe implements PipeTransform {
    constructor(private readonly catsService: CatsService) {}

    transform(value: string) {
      const id = +value;

      if (isNaN(id)) {
        throw new BadRequestException(`cats id malformed (value=${id})`);
      }

      const cat = this.catsService.findOne(id);

      if (!cat) {
        throw new NotFoundException();
      }

      return prop ? cat[prop] : cat;
    }
  }

  return mixin(GetCatFromIdPipe);
}
