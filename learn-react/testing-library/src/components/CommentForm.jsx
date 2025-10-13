import React, { useState } from 'react'

/**
 * @param {Object} props
 * @param {(value: { comment: string, author: string }) => void} [props.onSubmit]
 */
function CommentForm({ onSubmit = () => {} }) {
  const [comment, setComment] = useState('')
  const [author, setAuthor] = useState('')

  /** @param {React.FormEvent<HTMLFormElement>} e */
  function handleSubmit(e) {
    e.preventDefault()
    onSubmit({ comment, author })
    setComment('')
    setAuthor('')
  }

  return (
    <form className="comment-form" onSubmit={handleSubmit}>
      <textarea
        className="box"
        placeholder="Write something..."
        value={comment}
        onChange={e => setComment(e.target.value)}
      />
      <label htmlFor="author">Your Name</label>
      <input
        className="input-field"
        type="text"
        id="author"
        placeholder="Input value"
        value={author}
        onChange={e => setAuthor(e.target.value)}
      />
      <button className="button" disabled={!Boolean(comment && author)}>
        Add Comment
      </button>
    </form>
  )
}

export default CommentForm
