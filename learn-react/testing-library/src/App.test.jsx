import React from 'react'
import { render, fireEvent } from '@testing-library/react'

import App from 'App'

describe('adding comment', () => {
  const comment = { target: { value: 'test comment' } }
  const author = { target: { value: 'test author' } }

  it('users should see their comment', () => {
    const { getByPlaceholderText, getByLabelText, getByText } = render(<App />)

    fireEvent.change(getByPlaceholderText(/Write something/i), comment)
    fireEvent.change(getByLabelText(/Your name/i), author)
    fireEvent.click(getByText(/Add comment/i))

    expect(getByText(/test comment/i)).toBeInTheDocument()
    expect(getByText(/test author/i)).toBeInTheDocument()
  })
})
