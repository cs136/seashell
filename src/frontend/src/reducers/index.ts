/*
Code based off template from https://github.com/DimitriMikadze/express-react-redux-starter/
*/
import { combineReducers } from "redux";

import appStateReducer, {appStateReducerState} from "./appStateReducer";
import userReducer, {userReducerState} from "./userReducer";
import settingsReducer, {settingsReducerState} from "./settingsReducer";
import dialogReducer, {dialogReducerState} from "./dialogReducer";

export interface globalState {
  appState: appStateReducerState;
  user: userReducerState;
  settings: settingsReducerState;
  dialog: dialogReducerState;
};

const rootReducer = combineReducers<globalState>({
  appState: appStateReducer,
  settings: settingsReducer,
  user: userReducer,
  dialog: dialogReducer
});


export default rootReducer;
