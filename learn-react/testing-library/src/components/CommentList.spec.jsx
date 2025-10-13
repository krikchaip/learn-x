import React from 'react'
import { render } from '@testing-library/react'

import CommentList from 'components/CommentList'

describe('<CommentList />', () => {
  it('display a list of comment cards with their comment and author tag', () => {
    const comments = [
      {
        id: 1,
        comment: 'I do love writing tests',
        author: 'The Notester'
      },
      {
        id: 2,
        comment: 'Nothing is better than a good comment app',
        author: 'Comment Hater'
      }
    ]

    const { getByText } = render(<CommentList comments={comments} />)

    expect(getByText(/I do love writing tests/i)).toBeInTheDocument()
    expect(getByText(/The Notester/i)).toBeInTheDocument()
    expect(
      getByText(/Nothing is better than a good comment app/i)
    ).toBeInTheDocument()
    expect(getByText(/Comment Hater/i)).toBeInTheDocument()
  })
})
