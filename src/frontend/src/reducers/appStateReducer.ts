import {mergeBetter} from "../helpers/utils";
import {projectRef, fileRef} from "../types";
export interface appStateReducerState {[key: string]: any;
  projects: string[];
  currentProject?: {
    name: string;
    id: string;
    questions: string[];
    currentQuestion: {
      name: string;
      files: string[];
      currentFile: {
        name: string;
        content: string
      };
    };
  };
};
export interface appStateReducerAction {type: string; payload: any; };
export const appStateActions = {
  changeFileName: "file_change_name",
  changeFileContent: "file_change_content"
};



export default function appStateReducer(state: appStateReducerState = {currentProject: {name: "A1 Racket", id: "A1R", questions: ["q1", "q2"], currentQuestion: {name: "q1", files: ["main.c", "test.txt"], currentFile: {name: "main.c", content: "#include <stdio.h>\nint main(){\n\tprintf(\"Hello World!\");\n}"}}}, projects: ["A1 Racket", "A2 C"]}, action: appStateReducerAction) {
  switch (action.type) {
    case appStateActions.changeFileContent:
      return mergeBetter(state, {currentProject: { currentQuestion: {currentFile: {content: action.payload}}}});
    case appStateActions.changeFileName:
      return mergeBetter(state, {name: action.payload});
    default:
      return state;
  }
}