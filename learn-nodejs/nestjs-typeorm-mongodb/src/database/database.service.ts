import { Injectable, type Type } from '@nestjs/common';

import { CommonService } from 'src/common';

@Injectable()
export class DatabaseService {
  private collections: Record<string, any[]> = {};

  constructor(private readonly commonService: CommonService) {}

  select<T>(entity: Type<T>, query: [keyof T, T[keyof T]][] = []): T[] {
    if (!this.collections[entity.name]) {
      this.collections[entity.name] = [];
    }

    return this.collections[entity.name].filter((record: T) => {
      return query.every(([key, value]) => record[key] === value);
    }) as T[];
  }

  insert<T extends { id?: number }>(entity: Type<T>, record: T): boolean {
    if (!this.collections[entity.name]) {
      this.collections[entity.name] = [];
    }

    record.id = this.commonService.genNextIdx(
      this.collections[entity.name] as T[],
      'id',
    );

    this.collections[entity.name].push(record);

    return true;
  }

  update<T>(entity: Type<T>, query: [keyof T, T[keyof T]][], record: T): T[] {
    if (!this.collections[entity.name]) {
      this.collections[entity.name] = [];
    }

    const targetIndices = this.collections[entity.name]
      .map((_, idx) => idx)
      .filter((idx) => {
        const target = this.collections[entity.name][idx] as T;
        return query.every(([key, value]) => target[key] === value);
      });

    targetIndices.forEach((idx) => {
      this.collections[entity.name][idx] = {
        ...(this.collections[entity.name][idx] as T),
        ...record,
      };
    });

    return targetIndices.map((idx) => this.collections[entity.name][idx] as T);
  }

  delete<T>(entity: Type<T>, query: [keyof T, T[keyof T]][]): T[] {
    if (!this.collections[entity.name]) {
      this.collections[entity.name] = [];
    }

    const targetIndices = this.collections[entity.name]
      .map((_, idx) => idx)
      .filter((idx) => {
        const target = this.collections[entity.name][idx] as T;
        return query.every(([key, value]) => target[key] === value);
      });

    const targets = targetIndices.map((idx) => {
      return this.collections[entity.name][idx] as T;
    });

    this.collections[entity.name] = this.collections[entity.name].filter(
      (_, idx) => {
        return !targetIndices.includes(idx);
      },
    );

    return targets;
  }
}
