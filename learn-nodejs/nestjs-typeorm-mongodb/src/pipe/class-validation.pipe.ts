import {
  ArgumentMetadata,
  BadRequestException,
  Injectable,
  PipeTransform,
  Type,
} from '@nestjs/common';
import { plainToInstance } from 'class-transformer';
import { validate } from 'class-validator';

import { CommonService } from 'src/common';

@Injectable()
export class ClassValidationPipe implements PipeTransform {
  private readonly NATIVE_TYPES: Type[] = [
    String,
    Boolean,
    Number,
    Array,
    Object,
  ];

  constructor(private readonly commonService: CommonService) {}

  async transform(value: any, metadata: ArgumentMetadata): Promise<any> {
    const { metatype } = metadata;

    if (!metatype || this.isNativeType(metatype)) {
      return value;
    }

    // eslint-disable-next-line
    const object: object = plainToInstance(metatype, value);

    const errors = await validate(object);
    if (errors.length > 0) {
      const now = this.commonService.getNowString();
      errors.forEach((error) => console.log(`${now} ${error.toString()}`));

      throw new BadRequestException('Validation Failed', { cause: errors });
    }

    return value;
  }

  private isNativeType(metatype: Type<any>): boolean {
    return this.NATIVE_TYPES.includes(metatype);
  }
}
