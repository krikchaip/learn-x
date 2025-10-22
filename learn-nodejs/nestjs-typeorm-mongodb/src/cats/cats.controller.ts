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
} from '@nestjs/common';

import { HttpExceptionFilter } from 'src/filter';

import { CatsService } from './cats.service';
import { CreateCatDto } from './dto/create-cat.dto';
import { UpdateCatDto } from './dto/update-cat.dto';

@Controller('cats')
@UseFilters(HttpExceptionFilter)
export class CatsController {
  constructor(private readonly catsService: CatsService) {}

  @Post()
  create(@Body() createCatDto: CreateCatDto) {
    return this.catsService.create(createCatDto);
  }

  @Get()
  findAll() {
    return this.catsService.findAll();
  }

  @Get(':id')
  findOne(
    @Param('id', ParseIntPipe) id: number,
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
  update(
    @Param(
      'id', // pass in an in-place instance for customization purpose
      new ParseIntPipe({ errorHttpStatusCode: HttpStatus.NOT_ACCEPTABLE }),
    )
    id: number,
    @Body() updateCatDto: UpdateCatDto,
  ) {
    const cat = this.catsService.update(id, updateCatDto);

    if (!cat) {
      // works the same as the above
      throw new HttpException('Not Found', HttpStatus.NOT_FOUND);
    }

    return cat;
  }

  @Delete(':id')
  remove(@Param('id') id: string) {
    const cat = this.catsService.remove(+id);

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
}
