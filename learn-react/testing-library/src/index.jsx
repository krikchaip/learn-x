import React from 'react'
import ReactDOM from 'react-dom'

import 'styles.css'

import App from 'App'

const initialState = [
  {
    comment: 'I do love writing tests',
    author: 'The Notester'
  },
  {
    comment: 'Nothing is better than a good comment app',
    author: 'Comment Hater'
  }
]

ReactDOM.render(
  <App initialState={initialState} />,
  document.getElementById('root')
)
