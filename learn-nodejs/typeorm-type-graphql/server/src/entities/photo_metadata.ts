import { Entity, Column, OneToOne, JoinColumn } from 'typeorm'

import { CustomEntity } from 'lib/custom_entity'
import { Photo } from './photo'

@Entity()
export class PhotoMetadata extends CustomEntity {
  @Column('int')
  height: number

  @Column('int')
  width: number

  @Column()
  orientation: string

  @Column()
  compressed: boolean

  @Column()
  comment: string

  @OneToOne(of => Photo, 'metadata', { onDelete: 'CASCADE' })
  @JoinColumn()
  photo: Photo

  // @OneToOne(of => Photo, 'metadata')
  // photo: Photo
}
