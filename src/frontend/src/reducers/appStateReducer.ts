import {evolve} from 'ramda';
import {projectRef, fileRef} from '../types';
export interface appStateReducerState {[key: string]: any};
export interface appStateReducerAction {type: string, payload: appStateReducerState}
export const appStateActions = {
};
export default function appStateReducer(state:appStateReducerState = {}, action:appStateReducerAction) {
  switch (action.type) {
    
    default:
      return state;
  }
}
