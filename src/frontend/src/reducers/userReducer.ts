import {evolve} from 'ramda';
import {projectRef, fileRef} from '../types';
export interface userReducerState {[key: string]: any; 
    questid?: string;
};
export interface userReducerAction {type: string, payload: userReducerState}
export const appStateActions = {
};
export default function assignmentReducer(state:userReducerState = {questid: "kcpei"}, action:userReducerAction={type:null, payload:{}}) {
  switch (action.type) {
    default:
      return state;
  }
}