import { Entity, Column, OneToMany } from 'typeorm'

import { CustomEntity } from 'lib/custom_entity'
import { Photo } from './photo'

@Entity()
export class Author extends CustomEntity {
  @Column()
  name: string

  @OneToMany(of => Photo, 'author', { cascade: true })
  photos: Photo[]
}
