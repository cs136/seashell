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
  fileOpTarget?: S.FileBrief;
  projects: S.ProjectBrief[];
  runState?: number;
  currentProject?: {
    termWrite?: Function;
    termClear?: Function;
    name: string;
    id: string;
    questions: string[];
    currentQuestion?: {
      name: string;
      files: S.FileBrief[];
      runFile: string;
      openFiles: S.FileBrief[];
      diags: CompilerDiagnostic[];
      currentFile?: CurrentFile;
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
    fileOpTarget: undefined,
    projects: [],
    runState: 0,
    currentProject: undefined
  }, action: appStateReducerAction) {
  switch (action.type) {
    case appStateActions.setTerm:
      state = clone(state);
      if (state.currentProject) {
        state.currentProject.termWrite = action.payload.termWrite;
        state.currentProject.termClear = action.payload.termClear;
      } else {
        throw new Error("Inconsistent state reached -- currentProject is undefined in setTerm");
      }
      return state;
    case appStateActions.writeConsole:
      if (state.currentProject) {
        if (state.currentProject.termWrite) {
          state.currentProject.termWrite(action.payload.content);
        }
      } else {
        throw new Error("Inconsistent state reached -- currentProject is undefined in writeConsole");
      }
      return state;
    case appStateActions.clearConsole:
      if (state.currentProject) {
        if (state.currentProject.termClear) {
          state.currentProject.termClear();
        }
      } else {
        throw new Error("Inconsistent state reached -- currentProject is undefined in clearConsole");
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
      if (state.currentProject && state.currentProject.currentQuestion) {
        state.currentProject.currentQuestion.currentFile = undefined;
      } else {
        throw new Error("Invalid state reached -- currentProject or currentQuestion is undefined in invalidateFile");
      }
      return state;
    case appStateActions.getProjects:
      state = clone(state);
      state.projects = action.payload.projects;
      return state;
    case appStateActions.switchFile:
      state = clone(state);
      if (state.currentProject && state.currentProject.currentQuestion) {
        state.currentProject.currentQuestion.currentFile = action.payload;
      } else {
        throw new Error("Invalid state reached -- currentProject or currentQuestion is undefined in switchFile");
      }
      return state;
    case appStateActions.switchQuestion:
      state = clone(state);
      if (state.currentProject) {
        state.currentProject.currentQuestion = action.payload.question;
      } else {
        throw new Error("Invalid state reached -- currentProject is undefined in switchQuestion");
      }
      return state;
    case appStateActions.switchProject:
      state = clone(state);
      state.currentProject = action.payload.project;
      return state;
    // we will leave switching to a new project/question/file on deletion if necessary to the UI
    case appStateActions.removeQuestion:
      state = clone(state);
      if (state.currentProject) {
        state.currentProject.questions.splice(state.currentProject.questions.indexOf(action.payload.name), 1);
      } else {
        throw new Error("Invalid state reached -- currentProject is undefined in removeQuestion");
      }
      return state;
    case appStateActions.removeProject:
      // TODO: broken
      state = clone(state);
      if (state.projects) {
        state.projects.splice(state.projects.indexOf(action.payload.name), 1);
      }
      return state;
    case appStateActions.removeFile:
      state = clone(state);
      if (state.currentProject && state.currentProject.currentQuestion) {
        state.currentProject.currentQuestion.files.splice(state.currentProject.currentQuestion.files.indexOf(action.payload.name), 1);
      } else {
        throw new Error("Invalid state reached -- currentProject/Question is undefined in removeFile");
      }
      return state;
    case appStateActions.addQuestion:
      state = clone(state);
      if (state.currentProject) {
        state.currentProject.questions.push(action.payload.name);
      } else {
        throw new Error("Invalid state reached -- currentProject is undefined in addQuestion");
      }
      return state;
    case appStateActions.addProject:
      // TODO: make sure projectbrief is passed in
      state = clone(state);
      if (state.projects) {
        state.projects.push(action.payload);
      } else {
        state.projects = [action.payload];
      }
      return state;
    // leave switching project/question to the UI
    case appStateActions.addFile:
      state = clone(state);
      if (state.currentProject && state.currentProject.currentQuestion) {
        state.currentProject.currentQuestion.files.push(action.payload.name);
      } else {
        throw new Error("Inconsistent state reached -- currentProject/Question is undefined in addFile");
      }
      return state;
    case appStateActions.changeFileBufferedContent:
      const {unwrittenContent, target, flusher} = action.payload;
      if (state.currentProject &&
          state.currentProject.currentQuestion &&
          state.currentProject.currentQuestion.currentFile) {
        if (state.currentProject.currentQuestion.currentFile.flusher) {
          clearTimeout(state.currentProject.currentQuestion.currentFile.flusher);
          return mergeBetter(state,
          {currentProject:
            {currentQuestion:
              {currentFile:
                {unwrittenContent: unwrittenContent,
                target: target,
                flusher: setTimeout(flusher, 2500)}}}});
        }
      } else {
        throw new Error("Inconsistent state reached -- currentProject/Question/File is undefined in updateFile");
      }
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
      state = clone(state);
      if (state.currentProject && state.currentProject.currentQuestion) {
        if (state.currentProject.currentQuestion.openFiles.indexOf(action.payload) !== -1)
          return state; // don't duplicate files
        state.currentProject.currentQuestion.openFiles.push(action.payload);
      } else {
        throw new Error("Inconsistent state reached -- currentProject/Question is undefined in openFile");
      }
      return state;
    case appStateActions.closeFile:
      state = clone(state);
      if (state.currentProject && state.currentProject.currentQuestion) {
        state.currentProject.currentQuestion.openFiles = reject(equals(action.payload), state.currentProject.currentQuestion.openFiles);
      } else {
        throw new Error("Inconsistent state reached -- currentProject/Question is undefined in openFile");
      }
      return state;
    case appStateActions.setRunFile:
      state = clone(state);
      return mergeBetter(state, {currentProject: {currentQuestion: {runFile: action.payload}}});
    case appStateActions.setDiags:
      state = clone(state);
      if (state.currentProject && state.currentProject.currentQuestion) {
        state.currentProject.currentQuestion.diags = action.payload;
      } else {
        throw new Error("Inconsistent state reached -- currentProject/Question is undefined in setDiags");
      }
      return state;
    default:
      return state;
  }
}
