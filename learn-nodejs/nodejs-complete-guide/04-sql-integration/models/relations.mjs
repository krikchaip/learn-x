import { Cart } from './cart.mjs'
import { Product } from './product.mjs'
import User from './user.mjs'

User.hasMany(Product, {
  onUpdate: 'CASCADE',
  onDelete: 'CASCADE',
})
Product.belongsTo(User)

Product.hasMany(Cart, {
  onUpdate: 'CASCADE',
  onDelete: 'CASCADE',
})
Cart.belongsTo(Product)

User.belongsToMany(Product, { through: Cart })
Product.belongsToMany(User, { through: Cart })
