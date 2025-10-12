import express from 'express'

import Cart from '../models/cart.mjs'
import Product from '../models/product.mjs'

const router = express.Router()

router.get('/', (_, res) => {
  Product.fetchAll((products) => {
    res.render('shop/index', {
      title: 'Shop',
      path: '/',
      prods: products,
    })
  })
})

router.get('/products', (_, res) => {
  Product.fetchAll((products) => {
    res.render('shop/product-list', {
      title: 'All Products',
      path: '/products',
      prods: products,
    })
  })
})

router.get('/products/:id', (req, res) => {
  const { id } = req.params
  Product.findById(id, (product) => {
    res.render('shop/product-detail', {
      title: product.title,
      path: '/products',
      product,
    })
  })
})

router.get('/cart', (_, res) => {
  Cart.getCart((cart) => {
    Product.fetchAll((products) => {
      const cartProducts = []

      for (const product of products) {
        const cartProductData = cart.products.find(
          (prod) => prod.id === product.id
        )
        if (cartProductData) {
          cartProducts.push({ productData: product, qty: cartProductData.qty })
        }
      }

      res.render('shop/cart', {
        title: 'Your Cart',
        path: '/cart',
        products: cartProducts,
      })
    })
  })
})

router.post('/cart', (req, res) => {
  const { id } = req.body

  Product.findById(id, (p) => {
    Cart.addProduct(id, p.price)
  })

  res.redirect('/cart')
})

router.post('/cart-delete-item', (req, res) => {
  const prodId = req.body.productId
  Product.findById(prodId, (product) => {
    Cart.deleteProduct(prodId, product.price)
    res.redirect('/cart')
  })
})

router.get('/orders', (_, res) => {
  res.render('shop/orders', {
    title: 'Your Orders',
    path: '/orders',
  })
})

router.get('/checkout', (_, res) => {
  res.render('shop/checkout', {
    title: 'Checkout',
    path: '/checkout',
  })
})

export default router
