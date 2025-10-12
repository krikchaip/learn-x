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

router.get('/products', async (req, res) => {
  res.render('shop/product-list', {
    title: 'All Products',
    path: '/products',
    prods: await req.user.getProducts(),
  })
})

router.get('/products/:id', (req, res) => {
  Product.findById(req.params.id, (product) => {
    res.render('shop/product-detail', {
      title: product.title,
      path: '/products',
      product,
    })
  })
})

router.get('/cart', async (req, res) => {
  Cart.getCart(req.user.id, (products) => {
    res.render('shop/cart', {
      title: 'Your Cart',
      path: '/cart',
      products,
    })
  })
})

router.post('/cart', (req, res) => {
  Cart.addProduct(req.user.id, req.body.id, () => {
    res.redirect('/cart')
  })
})

router.post('/cart-delete-item', (req, res) => {
  Cart.deleteProduct(req.user.id, req.body.productId, () => {
    res.redirect('/cart')
  })
})

router.get('/checkout', (_, res) => {
  res.render('shop/checkout', {
    title: 'Checkout',
    path: '/checkout',
  })
})

export default router
