import {evolve} from 'ramda';
import {fileRef} from '../types';
export interface fileReducerState {[key: string]: any; 
    name?: string;
    content?: string;
};
export interface fileReducerAction {type: string, payload: fileReducerState}
export const fileActions = {
  changeName: 'file_change_name',
  changeContent: 'file_change_content'
};
export default function fileReducer(state:fileReducerState = {name: "default.c"}, action:fileReducerAction={type:null, payload:{}}) {
  switch (action.type) {
    case fileActions.changeContent:
      return evolve(state, {name: state.name, content: action.payload.content});
    case fileActions.changeName:
      return evolve(state, {name: action.payload.name, content: state.content});
    default:
      return state;
  }
}