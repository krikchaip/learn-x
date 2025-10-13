import React from 'react'
import { render } from '@testing-library/react'

import CommentCard from 'components/CommentCard'

describe('<CommentCard />', () => {
  const comment = 'React Testing Library is great'
  const author = 'Luke Ghenco'

  it("display comment with author's name", () => {
    const { getByText } = render(
      <CommentCard comment={comment} author={author} />
    )

    expect(getByText(/React Testing Library is great/i)).toBeVisible()
    expect(getByText(/Luke Ghenco/i)).toBeVisible()
  })
})
