/*
Code based off template from https://github.com/DimitriMikadze/express-react-redux-starter/
*/
import { combineReducers } from "redux";

import appStateReducer, {appStateReducerState} from "./appStateReducer";
import userReducer, {userReducerState} from "./userReducer";
import settingsReducer, {settingsReducerState} from "./settingsReducer";
import dialogReducer, {dialogReducerState} from "./dialogReducer";

const rootReducer = combineReducers({
  appState: appStateReducer,
  settings: settingsReducer,
  user: userReducer,
  dialog: dialogReducer
});

export interface globalState {
  appState: appStateReducerState;
  user: userReducerState;
  settings: settingsReducerState;
  dialog: dialogReducerState;
};

export default rootReducer;