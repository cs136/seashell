import {evolve} from "ramda";
import {projectRef, fileRef} from "../types";
export interface userReducerState {[key: string]: any;
    questid?: string;
};
export interface userReducerAction {type: string; payload: userReducerState; };
export const userActions = {
  INVALIDATE: "USER_INVALIDATE",
  SIGNIN: "USER_SIGNIN"
};
export default function userReducer(state: userReducerState = {questid: null}, action: userReducerAction= {type: null, payload: {}}) {
  switch (action.type) {
    case userActions.INVALIDATE:
      return {questid: null};
    case userActions.SIGNIN:
      return {questid: action.payload};
    default:
      return state;
  }
}