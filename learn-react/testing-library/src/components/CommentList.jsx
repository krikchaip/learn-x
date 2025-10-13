import React from 'react'

import CommentCard from './CommentCard'

/**
 * @typedef {object} Props
 * @property {{ comment: string, author: string }[]} comments
 */

/** @param {Props} props */
function CommentList(props) {
  const { comments } = props
  return (
    <div>
      {comments.map((c, idx) => (
        <CommentCard key={idx} comment={c.comment} author={c.author} />
      ))}
    </div>
  )
}

export default CommentList
