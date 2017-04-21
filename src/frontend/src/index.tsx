import * as React from "react";
import { render } from "react-dom";
import { Provider } from "react-redux";
import { createStore, applyMiddleware } from "redux";
import { HashRouter } from "react-router-dom";
import { install } from "offline-plugin/runtime";
import {actionsInterface} from "./actions";
import HotKeys from "./HotKeys";
import reducers from "./reducers";
import App from "./App";
import {Services} from "./helpers/Services";
import thunk from "redux-thunk";

((() => require("webcrypto-shim")).bind(window))(); //inject webcrypto polyfill into window scope

if ("serviceWorker" in navigator) {
  window.addEventListener("load", function() {
    navigator.serviceWorker.register("sw.js");
  });
}

const createStoreWithMiddleware = applyMiddleware(thunk)(createStore);
const store = createStoreWithMiddleware(reducers);

const rootEl = document.getElementById("root");
render(<HashRouter><Provider store={store}><App /></Provider></HashRouter>, rootEl);

Services.init(store.dispatch, {
  debugService: true,
  debugWebSocket: true,
  debugWebStorage: true
});


install();
