import {
  ArgumentMetadata,
  BadRequestException,
  Injectable,
  PipeTransform,
  Type,
  mixin,
} from '@nestjs/common';
import { ZodType } from 'zod';

import { CommonService } from 'src/common';

// creating nestjs pipe with a config object AND dependencies
// refs:
//   - https://stackoverflow.com/questions/62526828/how-to-create-a-nestjs-pipe-with-a-config-object-and-dependency
//   - https://github.com/microsoft/TypeScript/issues/30355
export function ZodValidationPipe(schema: ZodType): Type<PipeTransform> {
  @Injectable()
  class ValidationPipe implements PipeTransform {
    constructor(private readonly commonService: CommonService) {}

    // eslint-disable-next-line
    transform(value: any, _: ArgumentMetadata) {
      try {
        return schema.parse(value);
      } catch (error) {
        console.log(`${this.commonService.getNowString()} ${error}`);
        throw new BadRequestException('Validation Failed', { cause: error });
      }
    }
  }

  return mixin(ValidationPipe);
}
