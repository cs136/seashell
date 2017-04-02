import {mergeBetter} from "../helpers/utils";
import {clone, reject, equals} from "ramda";
import {projectRef, fileRef} from "../types";
export interface appStateReducerState {[key: string]: any;
  projects: string[];
  currentProject: {
    name: string;
    id: string;
    questions: string[];
    currentQuestion: {
      name: string
      files: string[];
      runFile: string;
      openFiles: string[];
      currentFile: {
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
  clearFiles: "file_clear",
  getProjects: "projects_get",
};



export default function appStateReducer(state: appStateReducerState = {currentProject: {name: "", id: "", questions: ["question"], currentQuestion: {name: "question", files: ["file1.txt"], runFile: "file.txt", openFiles: ["question/file1.txt"], currentFile: {name: "file.txt", content: "content"}}}, projects: ["A1"]}, action: appStateReducerAction) {
  switch (action.type) {
    case appStateActions.getProjects:
      state=clone(state);
      state.projects=action.payload.projects;
      return state;
    case appStateActions.clearFiles:
      state=clone(state);
      state.currentProject.currentQuestion.openFiles = [];
      return state;
    case appStateActions.switchFile:
      state=clone(state);
      state.currentProject.currentQuestion.currentFile=action.payload.file;
      return state;
    case appStateActions.switchQuestion:
      state=clone(state);
      state.currentProject.currentQuestion=action.payload.question;
      return state;
    case appStateActions.switchProject:
      state = clone(state);
      state.currentProject = action.payload.project;
      return state;
    // we will leave switching to a new project/question/file on deletion if necessary to the UI
    case appStateActions.removeQuestion:
      state=clone(state);
      state.currentProject.questions.splice(state.currentProject.questions.indexOf(action.payload.name), 1);
      return state;
    case appStateActions.removeProject:
      state=clone(state);
      state.projects.splice(state.projects.indexOf(action.payload.name), 1);
      return state;
    case appStateActions.removeFile:
      state = clone(state);
      state.currentProject.currentQuestion.files.splice(state.currentProject.currentQuestion.files.indexOf(action.payload.name), 1);
      return state;
    case appStateActions.addQuestion:
      state=clone(state);
      state.currentProject.questions.push(action.payload.name);
      return state;
    case appStateActions.addProject:
      state=clone(state);
      state.projects.push(action.payload.name);
      return state;
    // leave switching project/question to the UI
    case appStateActions.addFile:
      state = clone(state);
      state.currentProject.currentQuestion.files.push(action.payload.name);
      return state;
    case appStateActions.changeFileContent:
      return mergeBetter(state, {currentProject: { currentQuestion: {currentFile: {content: action.payload}}}});
    case appStateActions.openFile:
      if (state.currentProject.currentQuestion.openFiles.indexOf(action.payload) !== -1) return state; // don't duplicate files
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