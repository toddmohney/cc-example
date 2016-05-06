import 'babel-polyfill'

import React from 'react'
import { render } from 'react-dom'
import { Provider } from 'react-redux'
import { createStore } from 'redux'
import { meatbarApp } from './reducers'
import configureStore from './configureStore'
import App from './components/App'

let initialState = {
  meatbarEaters: [],
  consumedMeatbars: [],
  hasLoaded: false
}

let store = configureStore(initialState)

render(
  <Provider store={store}>
  <App />
  </Provider>,
  document.getElementById('root')
)
