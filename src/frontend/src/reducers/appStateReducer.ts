import {mergeBetter} from "../helpers/utils";
import {CompilerDiagnostic} from "../helpers/Services";
import {clone, reject, equals} from "ramda";
import {projectRef, fileRef} from "../types";
import * as S from "../helpers/Storage/Interface";

interface CurrentFile extends S.File {
  unwrittenContent?: string;
  target?: S.FileID;
  flusher?: number;
}

export interface appStateReducerState {[key: string]: any;
  fileOpTarget: S.FileBrief;
  projects: string[];
  runState: number;
  currentProject: {
    termWrite: Function;
    termClear: Function;
    name: string;
    id: string;
    questions: string[];
    currentQuestion: {
      name: string;
      files: S.FileBrief[];
      runFile: string;
      openFiles: S.FileBrief[];
      diags: CompilerDiagnostic[];
      currentFile: CurrentFile;
    };
  };
};

export interface appStateReducerAction {
  type: appStateActions;
  payload: any;
};

export enum appStateActions {
  changeFileContent,
  changeFileBufferedContent,
  addFile,
  addProject,
  addQuestion,
  removeFile,
  removeProject,
  removeQuestion,
  switchFile,
  switchQuestion,
  switchProject,
  renameFile,
  openFile,
  closeFile,
  setRunFile,
  copyFile,
  getProjects,
  invalidateFile,
  setFileOpTarget,
  setRunning,
  setCompiling,
  setNotRunning,
  setTerm,
  writeConsole,
  clearConsole,
  setDiags
};

export default function appStateReducer(state: appStateReducerState = {
    fileOpTarget: null,
    projects: [],
    runState: 0,
    currentProject: {
      termWrite: null,
      termClear: null,
      name: "",
      id: "",
      questions: [],
      currentQuestion: {
        name: "",
        files: [],
        runFile: "",
        openFiles: [],
        diags: [],
        currentFile: {
          id: "",
          name: "",
          project: "",
          checksum: "",
          last_modified: 0,
          contents: ""
        },
      },
    }
  }, action: appStateReducerAction) {
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
      state.fileOpTarget = action.payload;
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
      state.currentProject.currentQuestion.currentFile = action.payload;
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
    case appStateActions.changeFileBufferedContent:
      const {unwrittenContent, target, flusher} = action.payload;
      if (state.currentProject.currentQuestion.currentFile.flusher)
        clearTimeout(state.currentProject.currentQuestion.currentFile.flusher);
      return mergeBetter(state,
      {currentProject:
        {currentQuestion:
          {currentFile:
            {unwrittenContent: unwrittenContent,
             target: target,
             flusher: setTimeout(flusher, 2500)}}}});
    case appStateActions.changeFileContent:
      return mergeBetter(state,
      {currentProject:
        {currentQuestion:
          {currentFile:
            {content: action.payload,
              target: null,
              flusher: null,
              unwrittenContent: null}}}});
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
