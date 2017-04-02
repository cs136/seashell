import * as React from "react";
import { render } from "react-dom";
import { Provider } from "react-redux";
import { createStore, applyMiddleware } from "redux";
import { BrowserRouter } from "react-router-dom"
import { install } from "offline-plugin/runtime";
import {actionsInterface} from "./actions";
import HotKeys from "./HotKeys";
import reducers from "./reducers";
import App from "./App";
import {Services} from "./helpers/Services";

if ("serviceWorker" in navigator) {
  window.addEventListener("load", function() {
    navigator.serviceWorker.register("sw.js");
  });
}

const createStoreWithMiddleware = applyMiddleware()(createStore);
const store = createStoreWithMiddleware(reducers);

const rootEl = document.getElementById("root");
render(<BrowserRouter><Provider store={store}><App /></Provider></BrowserRouter>, rootEl);

Services.init(store.dispatch, {
  debugService: true,
  debugWebSocket: true,
  debugWebStorage: true
});

install();
