import React from 'react'

/**
 * @typedef {object} Props
 * @property {string} author
 * @property {string} comment
 */

/** @param {Props} props */
function CommentCard(props) {
  const { author, comment } = props
  return (
    <div className="comment-card">
      <div>{comment}</div>
      <div className="author-tag">- {author}</div>
    </div>
  )
}

export default CommentCard
