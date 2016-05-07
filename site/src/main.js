import 'babel-polyfill';

import React from 'react';
import { render } from 'react-dom';
import { Provider } from 'react-redux';
import configureStore from './configureStore';
import App from './components/App';

google.charts.load('current', { 'packages': ['corechart'] });
google.charts.setOnLoadCallback(runApp);

function runApp() {
  const initialState = {
    selectedEater: null,
    meatbarEaters: [],
    consumedMeatbars: [],
    hasLoaded: false
  };

  const store = configureStore(initialState);

  render(
    <Provider store={store}>
    <App />
    </Provider>,
    document.getElementById('root')
  );
}
