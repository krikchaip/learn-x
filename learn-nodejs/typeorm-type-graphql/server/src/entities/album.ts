import { Entity, Column, ManyToMany, JoinTable } from 'typeorm'

import { CustomEntity } from 'lib/custom_entity'
import { Photo } from './photo'

@Entity()
export class Album extends CustomEntity {
  @Column()
  name: string

  @ManyToMany(of => Photo, 'albums', { cascade: true })
  @JoinTable()
  photos: Photo[]
}
