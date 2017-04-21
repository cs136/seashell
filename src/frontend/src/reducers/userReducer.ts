import {evolve} from "ramda";
import {projectRef, fileRef} from "../types";
export interface userReducerState {[key: string]: any;
    questid?: string;
};
export interface userReducerAction {type: string; payload: userReducerState; };
export const userActions = {
  INVALIDATE: "USER_INVALIDATE",
  SIGNIN: "USER_SIGNIN",
  SIGNOUT: "USER_SIGNOUT"
};
export default function userReducer(state: userReducerState = {questid: undefined}, action: userReducerAction) {
  switch (action.type) {
    case userActions.INVALIDATE:
      return {questid: undefined};
    case userActions.SIGNIN:
      return {questid: action.payload};
    case userActions.SIGNOUT:
      return {questid: undefined};
    default:
      return state;
  }
}