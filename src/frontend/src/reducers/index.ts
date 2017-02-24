/*
Code based off template from https://github.com/DimitriMikadze/express-react-redux-starter/
*/
import { combineReducers } from "redux";

import appStateReducer, {appStateReducerState} from "./appStateReducer";
import projectListReducer, {projectListReducerState} from "./projectListReducer";
import userReducer, {userReducerState} from "./userReducer";
import settingsReducer, {settingsReducerState} from "./settingsReducer";

const rootReducer = combineReducers({
  appState: appStateReducer,
  settings: settingsReducer,
  projectList: projectListReducer,
  user: userReducer
});

export interface globalState {appState: appStateReducerState; projectList: projectListReducerState; user: userReducerState; settings: settingsReducerState; };

export default rootReducer;