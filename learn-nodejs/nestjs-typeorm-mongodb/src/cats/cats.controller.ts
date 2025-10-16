import {
  Controller,
  Get,
  Post,
  Body,
  Patch,
  Param,
  Delete,
  NotFoundException,
} from '@nestjs/common';

import { CatsService } from './cats.service';
import { CreateCatDto } from './dto/create-cat.dto';
import { UpdateCatDto } from './dto/update-cat.dto';

@Controller('cats')
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
  findOne(@Param('id') id: string) {
    const cat = this.catsService.findOne(+id);

    if (!cat) {
      throw new NotFoundException();
    }

    return cat;
  }

  @Patch(':id')
  update(@Param('id') id: string, @Body() updateCatDto: UpdateCatDto) {
    const cat = this.catsService.update(+id, updateCatDto);

    if (!cat) {
      throw new NotFoundException();
    }

    return cat;
  }

  @Delete(':id')
  remove(@Param('id') id: string) {
    const cat = this.catsService.remove(+id);

    if (!cat) {
      throw new NotFoundException();
    }

    return cat;
  }
}
