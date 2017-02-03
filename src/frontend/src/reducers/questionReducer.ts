import {evolve} from 'ramda';
import {fileReducerState} from './fileReducer';
import {questionRef, fileRef} from '../types';
export interface questionReducerState {[key: string]: any; 
    name?: string;
    files?: fileReducerState[];
};
export interface questionReducerAction {type: string, payload: questionReducerState}
export const appStateActions = {
  changeName: 'question_change_name',
};
export default function questionReducer(state:questionReducerState = {name: "default"}, action:questionReducerAction={type:null, payload:{}}) {
  switch (action.type) {
    case appStateActions.changeName:
      return evolve(state, {question: action.payload.name});
    default:
      return state;
  }
}