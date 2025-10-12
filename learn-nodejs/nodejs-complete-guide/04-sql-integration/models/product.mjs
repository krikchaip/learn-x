import Sequelize from 'sequelize'

import db from '../utils/database.mjs'
import sequelize from '../utils/sequelize.mjs'

const { DataTypes } = Sequelize

// ? Sequelize model
const ProductORM = sequelize.define('product', {
  id: {
    type: DataTypes.INTEGER,
    primaryKey: true,
    allowNull: false,
    autoIncrement: true,
  },
  title: { type: DataTypes.STRING, allowNull: false },
  price: { type: DataTypes.DOUBLE, allowNull: false },
  description: DataTypes.TEXT,
  imageUrl: { type: DataTypes.TEXT, allowNull: false },
})

class Product {
  constructor({ id, title, imageUrl, description, price }) {
    this.id = id
    this.title = title
    this.imageUrl = imageUrl
    this.description = description
    this.price = price
  }

  async save(cb) {
    // ? using mysql driver
    // // prettier-ignore
    // db.query(`
    //   INSERT INTO product(title, price, description, imageUrl)
    //   VALUE (?, ?, ?, ?)
    // `,
    //   [this.title, this.price, this.description, this.imageUrl],
    //   (err) => { if (err) console.log(err) }
    // )

    // ? using Sequelize model instance
    ProductORM.create({
      title: this.title,
      price: this.price,
      description: this.description,
      imageUrl: this.imageUrl,
    }).then(cb)
  }

  async update(cb) {
    // ? using mysql driver
    // // prettier-ignore
    // db.query(`
    //   UPDATE product SET
    //     title = ?,
    //     price = ?,
    //     description = ?,
    //     imageUrl = ?
    //   WHERE id = ?
    // `,
    //   [this.title, this.price, this.description, this.imageUrl, this.id],
    //   (err) => { if (err) console.log(err) }
    // )

    // ? using Sequelize model instance
    ProductORM.update(
      {
        title: this.title,
        price: this.price,
        description: this.description,
        imageUrl: this.imageUrl,
      },
      { where: { id: this.id } }
    ).then(cb)
  }

  static fetchAll(cb) {
    // ? using mysql driver
    // db.query(`SELECT * FROM product`, (err, results) => {
    //   if (err) console.log(err)
    //   cb(results)
    // })

    // ? using Sequelize model instance
    ProductORM.findAll().then(cb)
  }

  static findById(id, cb) {
    // ? using mysql driver
    // db.query(`SELECT * FROM product WHERE id = ?`, id, (err, results) => {
    //   if (err) console.log(err)
    //   cb(results[0])
    // })

    // ? using Sequelize model instance
    ProductORM.findByPk(id).then(cb)
  }

  static delete(id, cb) {
    // ? using mysql driver
    // db.query(`DELETE FROM product WHERE id = ?`, id, (err) => {
    //   if (err) console.log(err)
    //   cb()
    // })

    // ? using Sequelize model instance
    ProductORM.destroy({ where: { id } }).then(cb)
  }
}

export default Product
export { ProductORM as Product }
