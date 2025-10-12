import fs from 'fs'
import path from 'path'

import Cart from './Cart.mjs'

import { __rootdir } from '../utils/helpers.mjs'

const data = path.join(__rootdir, 'data', 'products.json')

const getProductsFromFile = (cb) => {
  fs.readFile(data, (err, fileContent) => {
    if (err) cb([])
    else cb(JSON.parse(fileContent))
  })
}

class Product {
  constructor({ id, title, imageUrl, description, price }) {
    this.id = id
    this.title = title
    this.imageUrl = imageUrl
    this.description = description
    this.price = price
  }

  save() {
    // editing
    if (this.id) {
      return getProductsFromFile((products) => {
        const idx = products.findIndex((p) => p.id === this.id)
        if (idx !== -1) {
          products[idx] = this
          fs.writeFile(data, JSON.stringify(products), (err) => {
            console.log(err)
          })
        }
      })
    }

    this.id = Math.random().toString()

    getProductsFromFile((products) => {
      products.push(this)
      fs.writeFile(data, JSON.stringify(products), (err) => {
        console.log(err)
      })
    })
  }

  static fetchAll(cb) {
    getProductsFromFile(cb)
  }

  static findById(id, cb) {
    getProductsFromFile((products) => {
      cb(products.find((p) => p.id === id))
    })
  }

  static delete(id, cb) {
    getProductsFromFile((products) => {
      const price = products.find((p) => p.id === id).price
      products = products.filter((p) => p.id !== id)
      fs.writeFile(data, JSON.stringify(products), (err) => {
        if (!err) {
          Cart.deleteProduct(id, price)
          cb()
        }
      })
    })
  }
}

export default Product
