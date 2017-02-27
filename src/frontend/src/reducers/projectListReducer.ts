import {evolve} from "ramda";
import {projectRef, fileRef} from "../types";
import projectReducer, {projectReducerState} from "./projectReducer";
export interface projectListReducerState {
  [key: string]: any;
  projects: projectReducerState[];
};
export interface projectListReducerAction {type: string; payload: projectListReducerState; };
export const appStateActions = {
  openProject: "state_open_project",
  openFile: "state_open_file"
};
export default function projectListReducer(state: projectListReducerState  = {projects: [{name: "A1 Racket", id: "A1R", questions: [{name: "q1", files: [{name: "main.c", content: "#include <stdio.h>\nint main(){\n\tprintf(\"Hello World!\");\n}"}]}, {name: "q2", files: [{name: "integrity2.txt", content: "Hello World 2"}]}]}, {name: "A2 C Functions", id: "A2C", questions: []}]}, action: projectListReducerAction) {
  switch (action.type) {
    /*case appStateActions.openProject:
      return evolve(state, {project: action.payload.project});
    case appStateActions.openFile:
      return evolve(state, {file: action.payload.file});*/
    default:
      return state;
  }
}
