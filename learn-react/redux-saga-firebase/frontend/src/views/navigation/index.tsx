import React from 'react'
import { Link } from 'react-router-dom'

import Menu from './menu'
import Navbar from './navbar'

// TODO: module testing
export const Navigation = () => {
  return (
    <Navbar>
      <Link to="/" className="brand-logo">
        MarioPlan
      </Link>
      <Menu />
    </Navbar>
  )
}

export default Navigation
