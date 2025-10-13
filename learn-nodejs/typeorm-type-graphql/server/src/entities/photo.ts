import {
  Entity,
  Column,
  OneToOne,
  JoinColumn,
  ManyToOne,
  ManyToMany
} from 'typeorm'

import { CustomEntity } from 'lib/custom_entity'
import { Album } from './album'
import { Author } from './author'
import { PhotoMetadata } from './photo_metadata'

@Entity()
export class Photo extends CustomEntity {
  @Column({ length: 100 })
  name: string

  @Column('text')
  description: string

  @Column()
  filename: string

  @Column('int', { default: 0 })
  views: number

  @Column()
  is_published: boolean

  @OneToOne(of => PhotoMetadata, 'photo', { cascade: true })
  metadata: PhotoMetadata

  // @OneToOne(of => PhotoMetadata, 'photo', { cascade: true })
  // @JoinColumn()
  // metadata: PhotoMetadata

  @ManyToOne(of => Author, 'photos', { onDelete: 'CASCADE' })
  @JoinColumn()
  author: Author

  @ManyToMany(of => Album, 'photos')
  albums: Album[]
}
