import {mergeBetter} from "../helpers/utils";
import {CompilerDiagnostic} from "../helpers/Services";
import {clone, reject, equals} from "ramda";
import {projectRef, fileRef} from "../types";

export interface appStateReducerState {[key: string]: any;
  fileOpTarget: string;
  projects: string[];
  runState: number;
  currentProject: {
    termWrite: Function;
    termClear: Function;
    name: string;
    id: string;
    questions: string[];
    currentQuestion: {
      name: string
      files: string[];
      runFile: string;
      openFiles: string[];
      diags: CompilerDiagnostic[];
      currentFile: {
        name: string;
        content: string
      };
    };
  };
};

export interface appStateReducerAction {
  type: string;
  payload: any;
};

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
  getProjects: "projects_get",
  invalidateFile: "file_invalidate",
  setFileOpTarget: "fileoptarget_set",
  setRunning: "set_running",
  setCompiling: "set_compiling",
  setNotRunning: "programDone",
  setTerm: "term_set",
  writeConsole: "console_write",
  clearConsole: "console_clear",
  setDiags: "set_diags"
};

export default function appStateReducer(state: appStateReducerState = {
    runState: 0,
    fileOpTarget: "",
    currentProject: {
      termWrite: null,
      termClear: null,
      name: "",
      id: "",
      questions: ["question"],
      currentQuestion: {
        name: "question",
        files: ["file1.txt"],
        runFile: "file.txt",
        openFiles: ["question/file1.txt"],
        diags: [],
        currentFile: {
          name: "file.txt",
          content: "content"
        }
      }
    }, projects: ["A1"]}, action: appStateReducerAction) {
  switch (action.type) {
    case appStateActions.setTerm:
      state = clone(state);
      state.currentProject.termWrite = action.payload.termWrite;
      state.currentProject.termClear = action.payload.termClear;
      return state;
    case appStateActions.writeConsole:
      if (state.currentProject.termWrite !== null) {
        state.currentProject.termWrite(action.payload.content);
      }
      return state;
    case appStateActions.clearConsole:
      if (state.currentProject.termClear !== null) {
        state.currentProject.termClear();
      }
      return state;
    case appStateActions.setRunning:
      state = clone(state);
      state.runState = 2;
      return state;
    case appStateActions.setNotRunning:
      state = clone(state);
      state.runState = 0;
      return state;
    case appStateActions.setCompiling:
      state = clone(state);
      state.runState = 1;
      return state;
    case appStateActions.setFileOpTarget:
      state = clone(state);
      state.fileOpTarget = action.payload.name;
      return state;
    case appStateActions.invalidateFile:
      state = clone(state);
      state.currentProject.currentQuestion.currentFile.name = "";
      return state;
    case appStateActions.getProjects:
      state = clone(state);
      state.projects = action.payload.projects;
      return state;
    case appStateActions.switchFile:
      state = clone(state);
      state.currentProject.currentQuestion.currentFile = action.payload.file;
      return state;
    case appStateActions.switchQuestion:
      state = clone(state);
      state.currentProject.currentQuestion = action.payload.question;
      return state;
    case appStateActions.switchProject:
      state = clone(state);
      state.currentProject = action.payload.project;
      return state;
    // we will leave switching to a new project/question/file on deletion if necessary to the UI
    case appStateActions.removeQuestion:
      state = clone(state);
      state.currentProject.questions.splice(state.currentProject.questions.indexOf(action.payload.name), 1);
      return state;
    case appStateActions.removeProject:
      state = clone(state);
      state.projects.splice(state.projects.indexOf(action.payload.name), 1);
      return state;
    case appStateActions.removeFile:
      state = clone(state);
      state.currentProject.currentQuestion.files.splice(state.currentProject.currentQuestion.files.indexOf(action.payload.name), 1);
      return state;
    case appStateActions.addQuestion:
      state = clone(state);
      state.currentProject.questions.push(action.payload.name);
      return state;
    case appStateActions.addProject:
      state = clone(state);
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
    case appStateActions.setDiags:
      state = clone(state);
      state.currentProject.currentQuestion.diags = action.payload;
      return state;
    default:
      return state;
  }
}
