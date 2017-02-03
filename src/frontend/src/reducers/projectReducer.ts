import {evolve} from 'ramda';
import {projectRef} from '../types';
import {questionReducerState} from './questionReducer';
export interface projectReducerState {[key: string]: any; 
    name?: string;
    id?: string;
    questions: questionReducerState[]
};
export interface projectReducerAction {type: string, payload: projectReducerState}
export const appStateActions = {
  changeName: 'project_change_name',
};
export default function projectReducer(state:projectReducerState, action:projectReducerAction={type:null, payload:{questions:[]}}) {
  switch (action.type) {
    case appStateActions.changeName:
      return evolve(state, {name: action.payload.name});
    default:
      return state;
  }
}