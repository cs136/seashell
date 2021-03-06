import {mergeBetter} from "../helpers/utils";
import {clone} from "ramda";
import {Action} from "redux";

export const dialogActions = {
  open: "dialog_open",
  close: "dialog_close",
  toggle: "dialog_toggle",
};

export interface dialogAction extends Action {
  type: string;
  payload: string;
}

export interface dialogReducerState {
  [key: string]: boolean;
  help_open: boolean;
  settings_open: boolean;
  add_project_open: boolean;
  delete_file_open: boolean;
  rename_file_open: boolean;
  copy_file_open: boolean;
  add_file_open: boolean;
  add_test_open: boolean;
  add_question_open: boolean;
  reset_open: boolean;
  archive_open: boolean;
}

export default function dialogReducer(
  state: dialogReducerState = {
    help_open: false,
    settings_open: false,
    add_project_open: false,
    delete_file_open: false,
    delete_project_open: false,
    rename_file_open: false,
    copy_file_open: false,
    add_file_open: false,
    add_test_open: false,
    add_question_open: false,
    reset_open: false,
    archive_open: false
  },
  action: dialogAction) {
  state = clone(state);
  switch (action.type) {
    case dialogActions.open:
      state[action.payload] = true;
      break;
    case dialogActions.close:
      state[action.payload] = false;
      break;
    case dialogActions.toggle:
      state[action.payload] = !state[action.payload];
      break;
  }
  return state;
}
