import {evolve} from 'ramda';
import {fileRef} from '../types';
export interface fileReducerState {[key: string]: any; 
    name?: string;
    content?: string;
};
export interface fileReducerAction {type: string, payload: fileReducerState}
export const appStateActions = {
  changeName: 'file_change_name',
};
export default function fileReducer(state:fileReducerState = {name: "default.c"}, action:fileReducerAction={type:null, payload:{}}) {
  switch (action.type) {
    case appStateActions.changeName:
      return evolve(state, {name: action.payload.name});
    default:
      return state;
  }
}