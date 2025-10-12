import Sequelize from 'sequelize'

import { Product } from './product.mjs'

import db from '../utils/database.mjs'
import sequelize from '../utils/sequelize.mjs'

const { DataTypes, QueryTypes } = Sequelize

// ? Sequelize model
const CartORM = sequelize.define('cart', {
  qty: {
    type: DataTypes.INTEGER,
    allowNull: false,
    defaultValue: 0,
  },
})

class Cart {
  static addProduct(userId, productId, cb) {
    // ? using mysql driver
    // // prettier-ignore
    // db.query(`
    //   INSERT INTO cart VALUE(?, 1)
    //   ON DUPLICATE KEY UPDATE qty = qty + 1
    // `, id, (err) => {
    //   if (err) throw err
    //   cb()
    // })

    // ? using Sequelize model instance
    // prettier-ignore
    sequelize.query(`
      INSERT INTO cart(userId, productId, qty) VALUE(?, ?, 1)
      ON DUPLICATE KEY UPDATE qty = qty + 1
    `, { type: QueryTypes.UPSERT, replacements: [userId, productId] }).then(cb)
  }

  static deleteProduct(userId, productId, cb) {
    // ? using mysql driver
    // // prettier-ignore
    // db.query(`DELETE FROM cart WHERE id = ?`, id, (err) => {
    //   if (err) throw err
    //   cb()
    // })

    // ? using Sequelize model instance
    CartORM.destroy({ where: { userId, productId } }).then(cb)
  }

  static async getCart(userId, cb) {
    // ? using mysql driver
    // // prettier-ignore
    // db.query(`
    //   SELECT product.*, cart.qty FROM cart
    //   JOIN product ON product.id = cart.id
    // `, (err, results) => {
    //   if (err) return cb(null)
    //   return cb(results.map(({ qty, ...productData }) =>({ productData, qty })))
    // })

    // ? using Sequelize model instance
    CartORM.findAll({
      where: { userId },
      include: Product,
    }).then((cart) => {
      // prettier-ignore
      cb(cart.map(({ qty, product: productData }) => ({
        productData,
        qty,
      })))
    })
  }
}

export default Cart
export { CartORM as Cart }
