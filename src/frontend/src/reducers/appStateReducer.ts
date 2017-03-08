import {mergeBetter} from "../helpers/utils";
import {projectRef, fileRef} from "../types";
export interface appStateReducerState {[key: string]: any;
  projects: string[];
  currentProject?: {
    name: string;
    id: string;
    questions: string[];
    currentQuestion?: {
      name: string
      files: string[];
      currentFile?: {
        name: string;
        content: string
      };
    };
  };
};
export interface appStateReducerAction {type: string; payload: any; };
export const appStateActions = {
  changeFileName: "file_change_name",
  changeFileContent: "file_change_content",
  addFile: "file_add",
  addProject: "project_add",
  addQuestion: "question_add",
  removeFile: "file_add",
  removeProject: "project_add",
  removeQuestion: "question_add"
};



export default function appStateReducer(state: appStateReducerState = {currentProject: {name: "A1 Racket", id: "A1R", questions: ["q1", "q2"], currentQuestion: {name: "q1", files: ["main.c", "test.txt"], currentFile: {name: "main.c", content: "#include <stdio.h>\nint main(){\n\tprintf(\"Hello World!\");\n}"}}}, projects: ["A1 Racket", "A2 C"]}, action: appStateReducerAction) {
  switch (action.type) {
    //we will leave switching to a new project/question/file on deletion if necessary to the UI
    case appStateActions.removeQuestion:
      return mergeBetter(state, {currentProject: {questions: state.currentProject.questions.splice(state.currentProject.questions.indexOf(action.payload.name), 1)}});
    case appStateActions.removeProject:
      return mergeBetter(state, {projects: state.projects.splice(state.projects.indexOf(action.payload.name), 1)});
    case appStateActions.removeFile:
      return mergeBetter(state, {currentProject: {currentQuestion: {files: state.currentProject.currentQuestion.files.splice(state.currentProject.currentQuestion.files.indexOf(action.payload.name), 1)}}});
    case appStateActions.addQuestion:
      return mergeBetter(state, {currentProject: {questions: state.currentProject.questions.push(action.payload.name), currentQuestion: {files: []}}});
    case appStateActions.addProject:
      return mergeBetter(state, {projects: state.projects.push(action.payload.name), currentProject: {questions: []}});
    case appStateActions.addFile:
      return mergeBetter(state, {currentProject: {currentQuestion: {files: state.currentProject.currentQuestion.files.push(action.payload.name),
                                                                    currentFile: {name: action.payload.name, content: action.payload.content}}}});
    case appStateActions.changeFileContent:
      return mergeBetter(state, {currentProject: { currentQuestion: {currentFile: {content: action.payload}}}});
    case appStateActions.changeFileName:
      return mergeBetter(state, {name: action.payload});
    default:
      return state;
  }
}