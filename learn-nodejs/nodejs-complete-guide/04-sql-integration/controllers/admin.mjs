import express from 'express'

import Product from '../models/product.mjs'

const router = express.Router()

router.get('/add-product', (_, res) => {
  res.render('admin/add-product', {
    title: 'Add Product',
    path: '/admin/add-product',
  })
})

router.post('/add-product', (req, res) => {
  const { title, imageUrl, price, description } = req.body
  req.user
    .createProduct({ title, imageUrl, description, price })
    .then(() => res.redirect('/admin/products'))
})

router.get('/edit-product/:id', (req, res) => {
  const { id } = req.params
  const { from } = req.query

  Product.findById(id, (product) => {
    if (!product) return res.redirect('/')
    res.render('admin/edit-product', {
      title: 'Edit Product',
      path: '/admin/add-product',
      redirect: from === 'user' ? '/products' : '/admin/products',
      product,
    })
  })
})

router.post('/edit-product/:id', (req, res) => {
  const { title, imageUrl, price, description, redirect } = req.body
  const { id } = req.params
  const product = new Product({ id, title, imageUrl, description, price })

  product.update(() => {
    res.redirect(redirect)
  })
})

router.post('/delete-product/:id', (req, res) => {
  Product.delete(req.params.id, () => {
    res.redirect(req.body.from === 'user' ? '/products' : '/admin/products')
  })
})

router.get('/products', (_, res) => {
  Product.fetchAll((products) => {
    res.render('admin/products', {
      title: 'Admin Products',
      path: '/admin/products',
      prods: products,
    })
  })
})

export default router
