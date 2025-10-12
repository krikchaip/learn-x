import express from 'express'

import Product from '../models/product.mjs'

const router = express.Router()

router.get('/', async (_, res) => {
  res.render('shop/index', {
    title: 'Shop',
    path: '/',
    prods: await Product.fetchAll(),
  })
})

router.get('/products', async (req, res) => {
  res.render('shop/product-list', {
    title: 'All Products',
    path: '/products',
    prods: await req.user.getProducts(),
  })
})

router.get('/products/:id', async (req, res) => {
  const product = await Product.findById(req.params.id)

  res.render('shop/product-detail', {
    title: product.title,
    path: '/products',
    product,
  })
})

router.get('/cart', async (req, res) => {
  res.render('shop/cart', {
    title: 'Your Cart',
    path: '/cart',
    products: await req.user.getCart(),
  })
})

router.post('/cart', async (req, res) => {
  await req.user.addToCart(req.body.id)
  res.redirect('/cart')
})

router.post('/cart-delete-item', async (req, res) => {
  await req.user.removeFromCart(req.body.productId)
  res.redirect('/cart')
})

router.get('/checkout', (_, res) => {
  res.render('shop/checkout', {
    title: 'Checkout',
    path: '/checkout',
  })
})

export default router
