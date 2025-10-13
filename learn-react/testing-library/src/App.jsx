import React, { useState } from 'react'

import CommentList from 'components/CommentList'
import CommentForm from 'components/CommentForm'

/**
 * @param {object} props
 * @param {{ comment: string, author: string }[]} [props.initialState]
 */
function App({ initialState = [] }) {
  const [comments, setComments] = useState(initialState)

  /**
   * @param {{ comment: string, author: string }} value
   */
  function addComment(value) {
    setComments(comments.concat(value))
  }

  return (
    <div>
      <CommentForm onSubmit={addComment} />
      <CommentList comments={comments} />
    </div>
  )
}

export default App
