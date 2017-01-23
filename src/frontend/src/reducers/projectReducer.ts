import {evolve} from 'ramda';
import {projectRef, fileRef} from '../types';
export interface projectReducerState {[key: string]: any; 
    name?: string;
    id?: number;
};
export interface projectReducerAction {type: string, payload: projectReducerState}
export const appStateActions = {
  changeName: 'project_change_name',
};
export default function projectReducer(state:projectReducerState = {name: "New Project", id:-1}, action:projectReducerAction={type:null, payload:{}}) {
  switch (action.type) {
    case appStateActions.changeName:
      return evolve(state, {project: action.payload.name});
    default:
      return state;
  }
}