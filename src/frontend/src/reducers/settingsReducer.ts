import {evolve} from "ramda";

export interface settingsReducerState {[key: string]: any;
  font?: string;
  fontSize?: number;
  editorMode?: number;
  tabWidth?: number;
  theme?: number;
  offlineMode?: number;
};
export const settingsActions = {
  updateSettings: "settings_update"
};

export interface settingsReducerAction {type: string; payload: settingsReducerState; };


export default function settingsReducer(state: settingsReducerState = {font: "Consolas", fontSize: 12, editorMode: 0, tabWidth: 1, theme: 0, offlineMode: 0}, action: settingsReducerAction = {type: "", payload: {}}) {
  switch (action.type) {
    case settingsActions.updateSettings:
      return evolve(state, action.payload);
    default:
      return state;
  }
}
