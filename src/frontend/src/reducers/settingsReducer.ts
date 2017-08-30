import {mergeBetter} from "../helpers/utils";
import {Action} from "redux";

export interface settingsReducerStateNullable {[key: string]: any;
  font?: string;
  fontSize?: number;
  editorMode?: number;
  tabWidth?: number;
  theme?: number;
  editorRatio?: number;
  updated?: number;
};

export interface settingsReducerState extends settingsReducerStateNullable {[key: string]: any;
  font: string;
  fontSize: number;
  editorMode: number;
  tabWidth: number;
  theme: number;
  editorRatio: number;
  updated: number;
};

export enum settingsActions {
  updateSettings =  "settings_update",
  updateEditorRatio =  "settings_editor_ratio_update",
  adjustFont = "adjust_font"
};

export interface settingsReducerAction extends Action {
  type: string;
  payload?: settingsReducerStateNullable | number;
};


export default function settingsReducer(
  state: settingsReducerState = {font: "Consolas",
    fontSize: 13,
    editorMode: 0,
    tabWidth: 1,
    theme: 0,
    editorRatio: 0.5,
    updated: 0},
  action: settingsReducerAction) {
  switch (action.type) {
    case settingsActions.updateSettings: {
      if (typeof action.payload !== "number") {
        let newState = mergeBetter(state, action.payload);
        newState.updated = (new Date()).getTime();
        if (newState.fontSize < 8) newState.fontSize = 8;
        return newState;
      } else {
        throw new Error("updateSettings was called with a number!");
      }
    }
    case settingsActions.updateEditorRatio: {
      let newState = mergeBetter(state, {editorRatio: action.payload});
      newState.updated = (new Date()).getTime();
      return newState;
    }
    case settingsActions.adjustFont: {
      if (typeof action.payload === "number") {
        let newState = mergeBetter(state, {fontSize: (state.fontSize || 0) + (action.payload || 0)});
        newState.updated = (new Date()).getTime();
        if (newState.fontSize < 8) newState.fontSize = 8;
        return newState;
      } else
        throw new Error("adjustFont was not passed a number!");
    }
    default:
      return state;
  }
}
