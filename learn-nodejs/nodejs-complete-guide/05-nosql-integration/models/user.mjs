import mongodb from 'mongodb'
import mongoose from 'mongoose'

import db from '../utils/database.mjs'

const { ObjectID } = mongodb
const { Schema, model } = mongoose

class User {
  // ? mongoose schema
  static Schema = new Schema(
    {
      __v: { select: false },
      name: String,
      email: String,
      cart: [
        {
          _id: false,
          productData: {
            type: Schema.Types.ObjectId,
            ref: 'product',
            alias: '_id',
          },
          qty: Number,
        },
      ],
    },
    { versionKey: false, collection: 'user' }
  )

  // ? mongoose model
  static Model = model('user', User.Schema)

  constructor({ id, name, email, cart = [] }) {
    if (id) {
      this.id = new ObjectID(id)
    }

    this.name = name
    this.email = email
    this.cart = cart
  }

  async create() {
    // ? using mongo driver
    // return db.user.insertOne(this)

    // ? using mongoose ODM
    return User.Model.create(this)
  }

  static async findById(id) {
    // ? using mongo driver
    // const { _id, ...data } = await db.user.findOne({ _id: new ObjectID(id) })
    // return new User({ id: _id, ...data })

    // ? using mongoose ODM
    return new User(await User.Model.findById(id))
  }

  async createProduct(product) {
    // ? using mongo driver
    // const prod = await product.save()
    // return db.product.updateOne(
    //   { _id: new ObjectID(prod.insertedId) },
    //   { $set: { userId: this.id } }
    // )

    // ? using mongoose ODM
    const p = await product.save()
    p.userId = this.id
    return p.save()
  }

  async getProducts() {
    // ? using mongo driver
    // return db.product
    //   .find({ userId: this.id })
    //   .project({ userId: false })
    //   .map(({ _id, ...data }) => ({ id: _id, ...data }))
    //   .toArray()

    // ? using mongoose ODM
    return model('product').find({ userId: this.id })
  }

  async getCart() {
    // ? using mongo driver
    // return db.user
    //   .aggregate([
    //     { $match: { _id: this.id } },
    //     { $unwind: '$cart' },
    //     { $replaceWith: '$cart' },
    //     {
    //       $lookup: {
    //         from: 'product',
    //         let: { productId: '$productData' },
    //         pipeline: [
    //           { $match: { $expr: { $eq: ['$_id', '$$productId'] } } },
    //           { $addFields: { id: '$_id' } },
    //           { $project: { _id: false, userId: false } },
    //         ],
    //         as: 'productData',
    //       },
    //     },
    //     { $unwind: '$productData' },
    //   ])
    //   .toArray()

    // ? using mongoose ODM
    return User.Model.findById(this.id)
      .populate('cart.productData', '-userId')
      .map(({ cart }) => cart)
  }

  async addToCart(productId) {
    // ? using mongo driver
    // const { modifiedCount } = await db.user.updateOne(
    //   { _id: this.id, 'cart.productData': new ObjectID(productId) },
    //   { $inc: { 'cart.$.qty': 1 } }
    // )

    // if (modifiedCount) return

    // return db.user.updateOne(
    //   { _id: this.id },
    //   { $push: { cart: { productData: new ObjectID(productId), qty: 1 } } }
    // )

    // ? using mongoose ODM
    const user = await User.Model.findById(this.id)
    const itemFound = user.cart.id(productId)

    if (itemFound) itemFound.qty++
    else user.cart.push({ productData: productId, qty: 1 })

    return user.save()
  }

  async removeFromCart(productId) {
    // ? using mongo driver
    // return db.user.updateOne(
    //   { _id: this.id },
    //   { $pull: { cart: { productData: new ObjectID(productId) } } }
    // )

    // ? using mongoose ODM
    return User.Model.updateOne(
      { _id: this.id },
      { $pull: { cart: { productData: productId } } }
    )
  }
}

export default User
