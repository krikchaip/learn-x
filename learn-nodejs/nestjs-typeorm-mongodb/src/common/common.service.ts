import { Injectable } from '@nestjs/common';

@Injectable()
export class CommonService {
  genNextIdx(collection: Record<string, any>[], key: string): number {
    const idx = collection.length + 1;

    const findNextIdx = (target: number) => {
      if (collection.find((record) => record[key] === target)) {
        return findNextIdx(target + 1);
      } else {
        return target;
      }
    };

    return findNextIdx(idx);
  }
}
