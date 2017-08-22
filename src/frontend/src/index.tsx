import * as React from "react";
import { render } from "react-dom";
import { Provider } from "react-redux";
import { createStore, applyMiddleware } from "redux";
import { HashRouter } from "react-router-dom";
import { install } from "offline-plugin/runtime";
import {actionsInterface} from "./actions";
import HotKeys from "./HotKeys";
import reducers from "./reducers";
import {userActions} from "./reducers/userReducer";
import App from "./App";
import {Services} from "./helpers/Services";
import thunk from "redux-thunk";
import {LoginRequired} from "./helpers/Errors";
import {getDispatch} from "./actions";
import {composeWithDevTools} from "redux-devtools-extension";
require("imports-loader?this=>window!webcrypto-shim"); // inject webcrypto polyfill into window scope

console.log(`Seashell ${VERSION} starting up.`);

if (PRODUCTION && "serviceWorker" in navigator) {
  require("offline-plugin/runtime").install();
}

const store = createStore(reducers, composeWithDevTools(
  applyMiddleware(thunk)));

const rootEl = document.getElementById("root");
render(<HashRouter><Provider store={store}><App /></Provider></HashRouter>, rootEl);

Services.init(store.dispatch, {
  debugService: true,
  debugWebSocket: true,
  debugLocalStorage: true,
  debugWebStorage: true
});


install();

// Try to autoconnect if possible.
(async () => {
  try {
    let dispatch = getDispatch(store.dispatch);
    await dispatch.dispatch.user.autoConnect();
    await dispatch.dispatch.project.getAllProjects();
    await dispatch.dispatch.settings.initSettings();
  } catch (err) {
    if (! (err instanceof LoginRequired)) {
      throw err;
    }
  }
})();
