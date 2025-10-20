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

  getNowString(): string {
    const now = new Date();

    const Y = now.getFullYear();
    const MM = now.getMonth();
    const dd = now.getDate();
    const HH = now.getHours().toString().padStart(2, '0');
    const mm = now.getMinutes().toString().padStart(2, '0');
    const ss = now.getSeconds().toString().padStart(2, '0');

    return `${Y}.${MM}.${dd} ${HH}:${mm}:${ss}`;
  }
}
