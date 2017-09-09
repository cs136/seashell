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
import {DebugLogs} from "./helpers/DebugLogs";
import {composeWithDevTools} from "redux-devtools-extension";
import * as Raven from "raven-js";
require("typeface-overpass");
require("imports-loader?this=>window!webcrypto-shim"); // Inject webcrypto polyfill into window scope

DebugLogs.enable();
console.log(`Seashell ${VERSION} starting up in ${DEBUG ? "debug" : "release"} mode for ${DOMAIN}.`);
console.log(`Type logs() into the JavaScript console to get frontend logs.`);

if (PRODUCTION && "serviceWorker" in navigator) {
  require("offline-plugin/runtime").install();
}

Raven.config("https://33e85fb4bc9341f492534ab43a80a463@sentry.io/189114").install();

const store = createStore(reducers, composeWithDevTools(
  applyMiddleware(thunk)));

const rootEl = document.getElementById("root");
render(<HashRouter><Provider store={store}><App /></Provider></HashRouter>, rootEl);

Services.init(store.dispatch, {
  debugService: DEBUG,
  debugWebSocket: DEBUG,
  debugLocalStorage: DEBUG,
  debugWebStorage: DEBUG
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
