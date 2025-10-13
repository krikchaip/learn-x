# TypeORM Entities

## 📏 Rules

- `snake_case` for filename 👉🏻 table name
- `snake_case` for class property 👉🏻 column name
- `PascalCase` for classname
- always extends `BaseEntity`
- ❌ no default exports
- 👍🏼 directly import from `index.ts`

## Examples

### `{ cascade: true }` on Photo

```ts
const photo = Photo.create({
  name: 'Me and Bears',
  description: 'I am near polar bears',
  filename: 'photo-with-bears.jpg',
  is_published: true,
  metadata: PhotoMetadata.create({
    width: 640,
    height: 480,
    compressed: true,
    comment: 'cybershoot',
    orientation: 'portrait'
  })
})

await photo.save()

console.log(await Photo.find({ relations: ['metadata'] }))
```

```ts
// Results
[
  Photo {
    id: '9f4ce0f8-2495-44e6-b081-94e5a2ca3e67',
    name: 'Me and Bears',
    description: 'I am near polar bears',
    filename: 'photo-with-bears.jpg',
    views: 0,
    is_published: true,
    metadata: PhotoMetadata {
      id: '67b9d26e-b3c8-4755-b846-08c5c9aa535e',
      height: 480,
      width: 640,
      orientation: 'portrait',
      compressed: true,
      comment: 'cybershoot'
    }
  }
]
```

### Create PhotoMetadata first, then Photo

```ts
const metadata = PhotoMetadata.create({
  width: 640,
  height: 480,
  compressed: true,
  comment: 'cybershoot',
  orientation: 'portrait',
  photo: Photo.create({
    name: 'Me and Bears',
    description: 'I am near polar bears',
    filename: 'photo-with-bears.jpg',
    is_published: true
  })
})

await metadata.photo.save()
await metadata.save()

console.log(await PhotoMetadata.find({ relations: ['photo'] }))
```

```ts
// Results
[
  PhotoMetadata {
    id: 'ab1caca6-ee37-42b6-a245-f7efee5a92a7',
    height: 480,
    width: 640,
    orientation: 'portrait',
    compressed: true,
    comment: 'cybershoot',
    photo: Photo {
      id: 'c0f577e2-2dc1-4311-9463-73d5c1f45b03',
      name: 'Me and Bears',
      description: 'I am near polar bears',
      filename: 'photo-with-bears.jpg',
      views: 0,
      is_published: true
    }
  }
]
```

### Custom query with QueryBuilder

```ts
console.log(
  await PhotoMetadata.createQueryBuilder('metadata')
    .innerJoinAndSelect('metadata.photo', 'photo')
    .select(['metadata.width', 'photo.name'])
    .getMany()
)
```

```ts
// Results
[
  PhotoMetadata {
    width: 640,
    photo: Photo {
      name: 'Me and Bears'
    }
  }
]
```

### Nested relation create with `{ cascade: true }` on Author

```ts
const author = await Author.create({
  name: 'Krikchai',
  photos: [
    Photo.create({
      name: 'Me and Bears',
      description: 'I am near polar bears',
      filename: 'photo-with-bears.jpg',
      is_published: true,
      metadata: PhotoMetadata.create({
        width: 640,
        height: 480,
        compressed: true,
        comment: 'cybershoot',
        orientation: 'portrait'
      })
    })
  ]
})

await author.save()

console.log(await Author.find({ relations: ['photos', 'photos.metadata'] }))
```

```ts
[
  Author {
    id: 'fb670a62-8d2f-4f7c-989b-cf68103d21d3',
    name: 'Krikchai',
    photos: [
      Photo {
        id: '15a08a38-72d9-4b51-92f5-ce98a3c17556',
        name: 'Me and Bears',
        description: 'I am near polar bears',
        filename: 'photo-with-bears.jpg',
        views: 0,
        is_published: true,
        metadata: PhotoMetadata {
          id: 'e6c1621e-8616-4a7e-a65b-9ab8e6e46b1a',
          height: 480,
          width: 640,
          orientation: 'portrait',
          compressed: true,
          comment: 'cybershoot'
        }
      }
    ]
  }
]
```

### Many-to-Many relation with `{ cascade: true }` on Photo

```ts
const photo = Photo.create({
  name: 'Me and Bears',
  description: 'I am near polar bears',
  filename: 'photo-with-bears.jpg',
  is_published: true,
  albums: [
    Album.create({ name: 'Bears' }),
    Album.create({ name: 'Me' }),
    Album.create({ name: 'You' })
  ]
})

await photo.save()

console.log(await Photo.find({ relations: ['albums'] }))
console.log(await Album.find({ relations: ['photos'] }))
```

```ts
// Results #1
[
  Photo {
    id: 'db34a302-8337-4632-8d24-ebe9815d657c',
    ...,
    albums: [
      Album { id: 'bd47f977-d675-40ca-967b-9a4227e0474d', name: 'Bears' },
      Album { id: '204a9a65-b70f-48f8-9fb0-241f8d156657', name: 'Me' },
      Album { id: '429f16cb-629b-4278-894b-d7119d79a323', name: 'You' }
    ]
  }
]

// Results #2
[
  Album {
    id: 'bd47f977-d675-40ca-967b-9a4227e0474d',
    name: 'Bears',
    photos: [
      Photo { id: 'db34a302-8337-4632-8d24-ebe9815d657c', ... }
    ]
  },
  Album {
    id: '204a9a65-b70f-48f8-9fb0-241f8d156657',
    name: 'Me',
    photos: [
      Photo { id: 'db34a302-8337-4632-8d24-ebe9815d657c', ... }
    ]
  },
  Album {
    id: '429f16cb-629b-4278-894b-d7119d79a323',
    name: 'You',
    photos: [
      Photo { id: 'db34a302-8337-4632-8d24-ebe9815d657c', ... }
    ]
  }
]
```
