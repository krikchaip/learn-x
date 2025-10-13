import React from 'react'
import { render, fireEvent } from '@testing-library/react'

import CommentForm from 'components/CommentForm'

describe('<CommentForm />', () => {
  it('button is initially disabled', () => {
    const { getByText } = render(<CommentForm />)

    expect(getByText(/Add Comment/i)).toBeDisabled()
  })

  describe('when both comment and author are filled', () => {
    const comment = { target: { value: 'test comment' } }
    const author = { target: { value: 'test author' } }

    it('submit button enabled', () => {
      const { getByText, getByLabelText, getByPlaceholderText } = render(
        <CommentForm />
      )

      fireEvent.change(getByPlaceholderText(/Write something/i), comment)
      fireEvent.change(getByLabelText(/Your Name/i), author)

      expect(getByText(/Add Comment/i)).toBeEnabled()
    })

    describe('clicking button', () => {
      it('would call a submit handler', () => {
        const handleSubmit = jest.fn()

        const { getByText, getByLabelText, getByPlaceholderText } = render(
          <CommentForm onSubmit={handleSubmit} />
        )

        fireEvent.change(getByPlaceholderText(/Write something/i), comment)
        fireEvent.change(getByLabelText(/Your Name/i), author)
        fireEvent.click(getByText(/Add Comment/i))

        expect(handleSubmit).toBeCalledWith({
          comment: 'test comment',
          author: 'test author'
        })
      })

      it('reset all form fields', () => {
        const { getByText, getByLabelText, getByPlaceholderText } = render(
          <CommentForm />
        )

        fireEvent.change(getByPlaceholderText(/Write something/i), comment)
        fireEvent.change(getByLabelText(/Your Name/i), author)
        fireEvent.click(getByText(/Add Comment/i))

        const textarea = /** @type {HTMLTextAreaElement} */ (getByPlaceholderText(
          /Write something/i
        ))
        const input = /** @type {HTMLInputElement} */ (getByLabelText(
          /Your Name/i
        ))

        expect(textarea.value).toBe('')
        expect(input.value).toBe('')
      })
    })
  })
})
