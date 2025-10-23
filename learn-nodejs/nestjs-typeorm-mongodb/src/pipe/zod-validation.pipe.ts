import {
  ArgumentMetadata,
  BadRequestException,
  Injectable,
  PipeTransform,
} from '@nestjs/common';
import { ZodType } from 'zod';

@Injectable()
export class ZodValidationPipe implements PipeTransform {
  constructor(private schema: ZodType) {}

  // eslint-disable-next-line
  transform(value: any, _: ArgumentMetadata) {
    try {
      return this.schema.parse(value); // eslint-disable-line
    } catch (error) {
      throw new BadRequestException('Validation Failed', { cause: error });
    }
  }
}
