import fs from 'fs'
import path from 'path'

import { __rootdir } from '../utils/helpers.mjs'

const data = path.join(__rootdir, 'data', 'cart.json')

class Cart {
  static addProduct(id, productPrice) {
    // Fetch the previous cart
    fs.readFile(data, (err, fileContent) => {
      let cart
      try {
        if (err) throw new Error('an error occured')

        cart = JSON.parse(fileContent)
        if (!cart.products || typeof cart.totalPrice !== 'number')
          throw new Error('invalid format')
      } catch {
        cart = { products: [], totalPrice: 0 }
      }

      // Analyze the cart => Find existing product
      const existingProductIndex = cart.products.findIndex(
        (prod) => prod.id === id
      )
      const existingProduct = cart.products[existingProductIndex]

      let updatedProduct

      // Add new product/ increase quantity
      if (existingProduct) {
        updatedProduct = { ...existingProduct }
        updatedProduct.qty = updatedProduct.qty + 1
        cart.products = [...cart.products]
        cart.products[existingProductIndex] = updatedProduct
      } else {
        updatedProduct = { id: id, qty: 1 }
        cart.products = [...cart.products, updatedProduct]
      }

      cart.totalPrice = cart.totalPrice + +productPrice

      fs.writeFile(data, JSON.stringify(cart), (err) => {
        console.log(err)
      })
    })
  }

  static deleteProduct(id, productPrice) {
    fs.readFile(data, (err, fileContent) => {
      if (err) return

      const updatedCart = { ...JSON.parse(fileContent) }
      const product = updatedCart.products.find((prod) => prod.id === id)
      if (!product) return
      const productQty = product.qty

      updatedCart.products = updatedCart.products.filter(
        (prod) => prod.id !== id
      )
      updatedCart.totalPrice =
        updatedCart.totalPrice - productPrice * productQty

      fs.writeFile(data, JSON.stringify(updatedCart), (err) => {
        console.log(err)
      })
    })
  }

  static getCart(cb) {
    fs.readFile(data, (err, fileContent) => {
      const cart = JSON.parse(fileContent)
      if (err) {
        cb(null)
      } else {
        cb(cart)
      }
    })
  }
}

export default Cart
