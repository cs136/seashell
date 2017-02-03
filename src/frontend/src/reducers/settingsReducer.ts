import {evolve} from 'ramda';
import {projectRef, fileRef} from '../types';
import projectReducer, {projectReducerState} from './projectReducer';
export interface settingsReducerState {
  constructor(font: string, fontSize:num, editorMode: num, tabWidth: num, theme: num, offlineMode: num){
    this.font=font;
    this.fontSize=fontSize;
    this.editorMode=editorMode;
    this.tabWidth=tabWidth;
    this.theme=theme;
    this.offlineMode=offlineMode;
  }
  font: string;
  fontSize: num
  editorMode: num
  tabWidth: num
  theme: num
  offlineMode: num
};
export interface settingsReducerAction {type: string, payload: settingsReducerState}

export default function settingsReducer(state:settingsReducerState , action:settingsReducerAction) {
  switch (action.type) {
    case appStateActions.changeFont:
      return evolve(state, {font: action.payload.font});
      break;
    case appStateActions.changeFontSize:
      return evolve(state, {fontSize: action.payload.fontSize});
      break;
    case appStateActions.changeEditorMode:
      return evolve(state, {editorMode: action.payload.editorMode});
      break;
    case appStateActions.changeTabWidth:
      return evolve(state, {tabWidth: action.payload.tabWidth});
      break;
    case appStateActions.changeTheme:
      return evolve(state, {theme: action.payload.theme});
      break;
    case appStateActions.changeOfflineMode:
      return evolve(state, {offlineMode: action.payload.offlineMode});
      break;
    default:
      return state;
  }
}
