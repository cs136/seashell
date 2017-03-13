import {mergeBetter} from "../helpers/utils";
import {clone, reject, equals} from "ramda";
import {projectRef, fileRef} from "../types";
export interface error {title: string, body: string};
export interface appStateReducerState {[key: string]: any;
  projects: string[];
  errors: error[];
  currentProject?: {
    name: string;
    id: string;
    questions: string[];
    currentQuestion?: {
      name: string
      files: string[];
      runFile?: string;
      openFiles: string[];
      currentFile?: {
        name: string;
        content: string
      };
    };
  };
};
export interface appStateReducerAction {type: string; payload: any; };
export const appStateActions = {
  changeFileContent: "file_change_content",
  addFile: "file_add",
  addProject: "project_add",
  addQuestion: "question_add",
  removeFile: "file_add",
  removeProject: "project_add",
  removeQuestion: "question_add",
  switchFile: "file_switch",
  switchQuestion: "question_switch",
  switchProject: "project_switch",
  renameFile: "file_rename",
  openFile: "file_open",
  closeFile: "file_close",
  setRunFile: "file_set_run",
  copyFile: "file_copy",
  removeError: "error_remove"
};



export default function appStateReducer(state: appStateReducerState = {errors: [{title: "Error title", body: "Error body"}, {title: "error title 2", body: "error body 2"}], currentProject: {name: "A1", id: "A1R", questions: ["q1", "q2"], currentQuestion: {name: "q1", files: ["main.c", "test.txt"], openFiles: [], runFile: null, currentFile: {name: "main.c", content: "#include <stdio.h>\nint main(){\n\tprintf(\"Hello World!\");\n}"}}}, projects: ["A1", "A2"]}, action: appStateReducerAction) {
  switch (action.type) {
    case appStateActions.removeError:
      state=clone(state);
      state.errors.splice(action.payload.errorIDX);
      return state
    case appStateActions.copyFile:
      return mergeBetter(state, {currentProject: {currentQuestion: mergeBetter(action.payload.question, {currentFile: mergeBetter(state.currentProject.currentQuestion.currentFile, {name: action.payload.newName})})}});
    case appStateActions.renameFile:
      return mergeBetter(state, {currentProject: {currentQuestion: mergeBetter(action.payload.question, {currentFile: mergeBetter(state.currentProject.currentQuestion.currentFile, {name: action.payload.newName})})}});
    case appStateActions.switchFile:
      return mergeBetter(state, {currentProject: {currentQuestion: {currentFile: action.payload.file}}});
    case appStateActions.switchQuestion:
      return mergeBetter(state, {currentProject: {currentQuestion: action.payload.question}});
    case appStateActions.switchProject:
      return mergeBetter(state, {currentProject: action.payload.project});
    // we will leave switching to a new project/question/file on deletion if necessary to the UI
    case appStateActions.removeQuestion:
      return mergeBetter(state, {currentProject: {questions: state.currentProject.questions.splice(state.currentProject.questions.indexOf(action.payload.name), 1)}});
    case appStateActions.removeProject:
      return mergeBetter(state, {projects: state.projects.splice(state.projects.indexOf(action.payload.name), 1)});
    case appStateActions.removeFile:
      state=clone(state);
      state.currentProject.currentQuestion.files.splice(state.currentProject.currentQuestion.files.indexOf(action.payload.name), 1);
      return state;
    case appStateActions.addQuestion:
      return mergeBetter(state, {currentProject: {questions: state.currentProject.questions.push(action.payload.name), currentQuestion: {files: []}}});
    case appStateActions.addProject:
      return mergeBetter(state, {projects: state.projects.push(action.payload.name), currentProject: {questions: []}});
    case appStateActions.addFile:
      return mergeBetter(state, {currentProject: {currentQuestion: {files: state.currentProject.currentQuestion.files.push(action.payload.name),
                                                                    currentFile: {name: action.payload.name, content: action.payload.content}}}});
    case appStateActions.changeFileContent:
      return mergeBetter(state, {currentProject: { currentQuestion: {currentFile: {content: action.payload}}}});
    case appStateActions.openFile:
      state = clone(state);
      state.currentProject.currentQuestion.openFiles.push(action.payload);
      return state;
    case appStateActions.closeFile:
      state = clone(state);
      state.currentProject.currentQuestion.openFiles = reject(equals(action.payload), state.currentProject.currentQuestion.openFiles);
      return state;
    case appStateActions.setRunFile:
      state = clone(state);
      return mergeBetter(state, {currentProject: {currentQuestion: {runFile: action.payload}}});
    default:
      return state;
  }
}