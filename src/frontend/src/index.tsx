import * as React from 'react';
import { render } from 'react-dom';
import { Provider } from 'react-redux';
import { createStore, applyMiddleware } from 'redux';
import { install } from 'offline-plugin/runtime';
import reducers from './reducers';
import App from './App';

if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('./uwseashell-service-worker.js');
}

const createStoreWithMiddleware = applyMiddleware()(createStore);
const store = createStoreWithMiddleware(reducers);

const rootEl = document.getElementById('root');
render(<Provider store={store}><App /></Provider>, rootEl);
 
install();
