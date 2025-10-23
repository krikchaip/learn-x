import { IsNumber, Min } from 'class-validator';

export class RemoveCatDto {
  @IsNumber({ allowInfinity: false, allowNaN: false })
  @Min(1)
  id: number;
}
