import mongodb from 'mongodb'
import mongoose from 'mongoose'

import db from '../utils/database.mjs'

const { ObjectID } = mongodb
const { Schema, model } = mongoose

class Product {
  // ? mongoose schema
  static Schema = new Schema(
    {
      __v: { select: false },
      title: String,
      imageUrl: String,
      description: String,
      price: String,
      userId: { type: Schema.Types.ObjectId, ref: 'user' },
    },
    { versionKey: false, collection: 'product' }
  )

  // ? mongoose model
  static Model = model('product', Product.Schema)

  constructor({ id, title, imageUrl, description, price }) {
    if (id) {
      this.id = new ObjectID(id)
    }

    this.title = title
    this.imageUrl = imageUrl
    this.description = description
    this.price = price
  }

  async save() {
    // ? using mongo driver
    // return db.product.insertOne(this)

    // ? using mongoose ODM
    return Product.Model.create(this)
  }

  async update() {
    // ? using mongo driver
    // return db.product.updateOne(
    //   { _id: this.id },
    //   {
    //     $set: {
    //       title: this.title,
    //       imageUrl: this.imageUrl,
    //       description: this.description,
    //       price: this.price,
    //     },
    //   }
    // )

    // ? using mongoose ODM
    return Product.Model.updateOne().where('_id').equals(this.id).set({
      title: this.title,
      imageUrl: this.imageUrl,
      description: this.description,
      price: this.price,
    })
  }

  static async fetchAll() {
    // ? using mongo driver
    // return db.product
    //   .find()
    //   .map(({ _id, ...data }) => ({ id: _id, ...data }))
    //   .toArray()

    // ? using mongoose ODM
    return Product.Model.find()
  }

  static async findById(id) {
    // ? using mongo driver
    // const { _id, ...data } = await db.product.findOne({ _id: new ObjectID(id) })
    // return { id: _id, ...data }

    // ? using mongoose ODM
    return Product.Model.findById(id)
  }

  static async delete(_id) {
    // ? using mongo driver
    // await Promise.all([
    //   db.product.deleteOne({ _id: new ObjectID(id) }),
    //   db.user.updateMany(
    //     {},
    //     { $pull: { cart: { productData: new ObjectID(id) } } }
    //   ),
    // ])

    // ? using mongoose ODM
    await Promise.all([
      model('product').deleteOne({ _id }),
      model('user').updateMany({}, { $pull: { cart: { productData: _id } } }),
    ])

    return
  }
}

export default Product
