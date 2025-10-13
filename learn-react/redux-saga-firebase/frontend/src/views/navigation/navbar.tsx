import React, { ReactNode } from 'react'

type Props = {
  children?: ReactNode
}

export const Navbar = (props: Props) => {
  const { children } = props
  return (
    <nav className="nav-wrapper grey darken-3">
      <div className="container">{children}</div>
    </nav>
  )
}

export default Navbar
