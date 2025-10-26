import {
  Controller,
  Get,
  Post,
  Body,
  Patch,
  Param,
  Delete,
  NotFoundException,
  HttpException,
  HttpStatus,
  UseFilters,
  ParseIntPipe,
  Query,
  ParseUUIDPipe,
  UsePipes,
  DefaultValuePipe,
  UseGuards,
  UseInterceptors,
} from '@nestjs/common';

import { HttpExceptionFilter } from 'src/filter';
import { ZodValidationPipe, ClassValidationPipe } from 'src/pipe';
import { RolesGuard } from 'src/guard';
import { Roles } from 'src/decorator';
import { LoggingInterceptor, MapResponseInterceptor } from 'src/interceptor';

import { CatsService } from './cats.service';
import { type CreateCatDto, createCatSchema } from './dto/create-cat.dto';
import { UpdateCatDto } from './dto/update-cat.dto';
import { RemoveCatDto } from './dto/remove-cat.dto';
import { Cat } from './cats.decorator';
import { Cat as CatEntity } from './entities/cat.entity';

@Controller('cats')
@UseFilters(HttpExceptionFilter)
@UseGuards(RolesGuard)
@Roles(['public'])
@UseInterceptors(MapResponseInterceptor)
export class CatsController {
  constructor(private readonly catsService: CatsService) {}

  @Post()
  @UsePipes(ZodValidationPipe(createCatSchema)) // apply this pipe to ALL params, one-by-one
  create(@Body() createCatDto: CreateCatDto) {
    return this.catsService.create(createCatDto);
  }

  @Get()
  @UseInterceptors(LoggingInterceptor)
  findAll(
    // set the default value to '1' when the following happen
    // - error occurs during parsing
    // - upon receiving `null`/`undefined` value
    @Query('page', new DefaultValuePipe(1), ParseIntPipe) page: number,
  ) {
    console.log(`page: ${page} (${typeof page})`);

    return this.catsService.findAll();
  }

  @Get(':id')
  @UseInterceptors(LoggingInterceptor)
  findOne(
    @Param('id', ParseIntPipe) id: number, // using pipe on a controller method's param
    @Query('uuid', ParseUUIDPipe) uuid?: string, // UUID is supported by default
  ) {
    if (uuid) {
      console.log(`[/cats/:id] uuid: ${uuid}`);
    }

    const cat = this.catsService.findOne(id);

    if (!cat) {
      // response: { "message": "Not Found", "statusCode": 404 }
      throw new NotFoundException();
    }

    return cat;
  }

  @Patch(':id')
  @Roles(['admin'])
  update(
    @Param(
      'id', // pass in an in-place instance for customization purpose
      new ParseIntPipe({ errorHttpStatusCode: HttpStatus.NOT_ACCEPTABLE }),
    )
    id: number,

    @Body(ClassValidationPipe)
    updateCatDto: UpdateCatDto,
  ) {
    const cat = this.catsService.update(id, updateCatDto);

    if (!cat) {
      // works the same as the above
      throw new HttpException('Not Found', HttpStatus.NOT_FOUND);
    }

    return cat;
  }

  @Delete(':id')
  @Roles(['admin'])
  remove(@Param() { id }: RemoveCatDto) {
    const cat = this.catsService.remove(id);

    if (!cat) {
      // overriding the entire response body
      // response: { "status": 404, "error": "cats with id=1 is not found" }
      throw new HttpException(
        {
          status: HttpStatus.NOT_FOUND,
          error: `cats with id=${id} is not found`,
        },
        HttpStatus.NOT_FOUND,
        { cause: new Error(`[original error] cats ${id} not found`) },
      );
    }

    return cat;
  }

  @Post('age')
  ageUp(@Cat() cat: CatEntity) {
    cat = this.catsService.update(cat.id, { age: cat.age + 1 })!;
    return cat;
  }
}
