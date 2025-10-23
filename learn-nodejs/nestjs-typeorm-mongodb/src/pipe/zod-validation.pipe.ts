import {
  ArgumentMetadata,
  BadRequestException,
  Injectable,
  PipeTransform,
} from '@nestjs/common';
import { ZodSchema } from 'zod/v3';

@Injectable()
export class ZodValidationPipe implements PipeTransform {
  constructor(private schema: ZodSchema) {}

  // eslint-disable-next-line
  transform(value: any, _: ArgumentMetadata) {
    try {
      return this.schema.parse(value); // eslint-disable-line
    } catch (error) {
      throw new BadRequestException('Validation Failed', { cause: error });
    }
  }
}
