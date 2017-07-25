import {cloneBetter as clone, mergeBetter} from "../helpers/utils";
import {CompilerDiagnostic} from "../helpers/Services";
import {reject, equals, find, propEq} from "ramda";
import {projectRef, fileRef} from "../types";
import * as S from "../helpers/Storage/Interface";
import {Action} from "redux";
import {showInfo} from "../partials/Errors";

class CurrentFile extends S.FileEntry {
  public unwrittenContent?: string;
  public target?: S.FileID;
  public flusher?: number;
  public versions: S.Contents[];

  constructor(other: CurrentFile | S.FileEntry, versions: S.Contents[]) {
    super(other);
    if (versions) {
      this.versions = versions;
    }
    if (other instanceof CurrentFile) {
      this.unwrittenContent = other.unwrittenContent;
      this.target = other.target;
      this.flusher = other.flusher;
      this.versions = other.versions;
    }
  }

  public clone(): CurrentFile {
    let result: CurrentFile = new CurrentFile(this, []);
    return result;
  }
}

export interface appStateReducerProjectState {
  termWrite?: Function;
  termClear?: Function;
  consoleText?: string;
  name: string;
  id: string;
  questions: string[];
  currentQuestion?: {
    name: string;
    files: string[];
    runFile: string;
    openFiles: string[];
    diags: CompilerDiagnostic[];
    currentFile?: CurrentFile;
  };
};

export interface appStateReducerState {
  [key: string]: any;
  fileOpTarget?: string;
  conflictContents: S.Contents[];
  projects: S.Project[];
  marmosetProjects: S.MarmosetProject[];
  marmosetInterval: number;
  runState?: number;
  currentProject?: appStateReducerProjectState;
  connected: boolean;
};

export interface appStateReducerAction extends Action {
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
  setMarmosetProjects,
  setMarmosetInterval,
  clearMarmosetInterval,
  invalidateFile,
  setFileOpTarget,
  setRunning,
  setCompiling,
  setNotRunning,
  setTerm,
  writeConsole,
  clearConsole,
  setDiags,
  updateCurrentFileIfNameEquals,
  connected,
  disconnected,
  applyServerChanges,
  conflictOccurred,
  conflictResolved
};

export default function appStateReducer(state: appStateReducerState = {
    fileOpTarget: undefined,
    conflictContents: [],
    projects: [],
    marmosetProjects: [],
    marmosetInterval: 0,
    runState: 0,
    currentProject: undefined,
    connected: false
  }, action: appStateReducerAction) {
  switch (action.type) {
    // This updates the current file if we're renaming
    // the current file or otherwise changing its name.
    case appStateActions.updateCurrentFileIfNameEquals:
      let {oldName, newFile} = <{oldName: string,
                                 newFile: S.FileEntry}>action.payload;
      state = clone(state);
      if (state.currentProject &&
          state.currentProject.currentQuestion &&
          state.currentProject.currentQuestion.currentFile &&
          state.currentProject.currentQuestion.currentFile.name === oldName) {
            state.currentProject.currentQuestion.currentFile.mergeIdFrom(newFile);
          }
      else
        console.warn("Inconsistent state reached -- currentFile is undefined in updateCurrentFile...");
      return state;
    case appStateActions.setTerm:
      state = clone(state);
      if (state.currentProject) {
        state.currentProject.termWrite = action.payload.termWrite;
        state.currentProject.termClear = action.payload.termClear;
      } else {
        console.warn("Inconsistent state reached -- currentProject is undefined in setTerm");
        // throw new Error("Inconsistent state reached -- currentProject is undefined in setTerm");
      }
      return state;
    case appStateActions.writeConsole:
      if (state.currentProject) {
        if (state.currentProject.termWrite) {
          state.currentProject.termWrite(action.payload.content);
          state.currentProject.consoleText += action.payload.content;
        }
      } else {
        console.warn("Inconsistent state reached -- currentProject is undefined in writeConsole");
        // throw new Error("Inconsistent state reached -- currentProject is undefined in writeConsole");
      }
      return state;
    case appStateActions.clearConsole:
      if (state.currentProject) {
        if (state.currentProject.termClear) {
          state.currentProject.termClear();
          state.currentProject.consoleText = "";
        }
      } else {
        console.warn("Inconsistent state reached -- currentProject is undefined in clearConsole");
        // throw new Error("Inconsistent state reached -- currentProject is undefined in clearConsole");
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
        console.warn("Invalid state reached -- currentProject or currentQuestion is undefined in invalidateFile");
        // throw new Error("Invalid state reached -- currentProject or currentQuestion is undefined in invalidateFile");
      }
      return state;
    case appStateActions.getProjects:
      state = clone(state);
      state.projects = action.payload.projects;
      return state;
    case appStateActions.setMarmosetProjects:
      state = clone(state);
      state.marmosetProjects = action.payload;
      return state;
    case appStateActions.setMarmosetInterval:
      state = clone(state);
      state.marmosetInterval = action.payload;
      return state;
    case appStateActions.switchFile:
      state = clone(state);
      if (state.currentProject && state.currentProject.currentQuestion) {
        if (action.payload.file instanceof S.FileEntry) {
          state.currentProject.currentQuestion.currentFile =
            new CurrentFile(action.payload.file, action.payload.versions);
        } else {
          console.error("switchFile was not passed a file entry:", action.payload);
        }
      } else {
        console.warn("Invalid state reached -- currentProject or currentQuestion is undefined in switchFile");
        // throw new Error("Invalid state reached -- currentProject or currentQuestion is undefined in switchFile");
      }
      return state;
    case appStateActions.switchQuestion:
      state = clone(state);
      if (state.currentProject) {
        state.currentProject.currentQuestion = action.payload.question;
      } else {
        console.warn("Invalid state reached -- currentProject is undefined in switchQuestion");
        // throw new Error("Invalid state reached -- currentProject is undefined in switchQuestion");
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
        console.warn("Invalid state reached -- currentProject is undefined in removeQuestion");
        // throw new Error("Invalid state reached -- currentProject is undefined in removeQuestion");
      }
      return state;
    case appStateActions.removeProject:
      state = clone(state);
      if (state.projects) {
        state.projects = state.projects.filter((project) => project.id !== action.payload.id);
      }
      return state;
    case appStateActions.removeFile:
      state = clone(state);
      let removeFile = <S.File>action.payload;
      if (state.currentProject && state.currentProject.currentQuestion) {
        let files = state.currentProject.currentQuestion.files;
        state.currentProject.currentQuestion.files =
          reject((file) => file === removeFile.name, files);
      } else {
        console.warn("Invalid state reached -- currentProject/Question is undefined in removeFile");
        // throw new Error("Invalid state reached -- currentProject/Question is undefined in removeFile");
      }
      return state;
    case appStateActions.addQuestion:
      state = clone(state);
      if (state.currentProject) {
        state.currentProject.questions.push(action.payload.name);
      } else {
        console.warn("Invalid state reached -- currentProject is undefined in addQuestion");
        // throw new Error("Invalid state reached -- currentProject is undefined in addQuestion");
      }
      return state;
    case appStateActions.addProject:
      // TODO: make sure project is passed in
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
        console.warn("Inconsistent state reached -- currentProject/Question is undefined in addFile");
        // throw new Error("Inconsistent state reached -- currentProject/Question is undefined in addFile");
      }
      return state;
    case appStateActions.changeFileBufferedContent:
      const {unwrittenContent, target, flusher} = action.payload;
      if (state.currentProject &&
          state.currentProject.currentQuestion &&
          state.currentProject.currentQuestion.currentFile) {
        if (state.currentProject.currentQuestion.currentFile.flusher)
          clearTimeout(state.currentProject.currentQuestion.currentFile.flusher);
        return mergeBetter(state,
          {currentProject:
            {currentQuestion:
              {currentFile:
                {unwrittenContent: unwrittenContent,
                target: target,
                flusher: setTimeout(flusher, 2500)}}}});
      } else {
        console.warn("Inconsistent state reached -- currentProject/Question/File is undefined in updateFile");
        // throw new Error("Inconsistent state reached -- currentProject/Question/File is undefined in updateFile");
      }
    case appStateActions.changeFileContent:
      return mergeBetter(state, {
        currentProject: {
          currentQuestion: {
            currentFile: {
              id: action.payload.id,
              contents: {
                contents: action.payload.contents
              },
              target: undefined,
              flusher: undefined,
              unwrittenContent: undefined
            }
          }
        }
      });
    case appStateActions.openFile:
      state = clone(state);
      if (state.currentProject && state.currentProject.currentQuestion) {
        if (state.currentProject.currentQuestion.openFiles.find((ofile) =>
              ofile === action.payload) !== undefined) {
          return state; // don't duplicate files
        }
        state.currentProject.currentQuestion.openFiles.push(action.payload);
      } else {
        console.warn("Inconsistent state reached -- currentProject/Question is undefined in openFile");
        // throw new Error("Inconsistent state reached -- currentProject/Question is undefined in openFile");
      }
      return state;
    case appStateActions.closeFile:
      state = clone(state);
      let oldFile = action.payload;
      if (state.currentProject && state.currentProject.currentQuestion) {
        state.currentProject.currentQuestion.openFiles =
          reject((file) => file === oldFile, state.currentProject.currentQuestion.openFiles);
      } else {
        console.warn("Inconsistent state reached -- currentProject/Question is undefined in openFile");
        // throw new Error("Inconsistent state reached -- currentProject/Question is undefined in openFile");
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
        console.warn("Inconsistent state reached -- currentProject/Question is undefined in setDiags");
        // throw new Error("Inconsistent state reached -- currentProject/Question is undefined in setDiags");
      }
      return state;
    case appStateActions.connected:
      state = clone(state);
      return mergeBetter(state, {connected: true});
    case appStateActions.disconnected:
      state = clone(state);
      return mergeBetter(state, {connected: false});
    case appStateActions.conflictOccurred:
      state = clone(state);
      state.conflictContents = action.payload;
      return state;
    case appStateActions.conflictResolved:
      state = clone(state);
      state.conflictContents = [];
      return state;
    case appStateActions.applyServerChanges:
      state = clone(state);
      const findIndex = <T>(arr: T[], pred: (x: T) => boolean): number => {
        for (let i in arr) {
          if (pred(arr[i]))
            return parseInt(i);
        }
        return -1;
      };
      let chgs = action.payload;
      let pchgs = chgs.filter((chg: any) => chg.table === "projects");
      // update the state's project list
      for (let i in pchgs) {
        if (pchgs[i].type === S.ChangeType.UPDATE) {
          let cur = findIndex(state.projects, (proj) => pchgs[i].key === proj.id);
          if (cur !== -1) {
            state.projects[cur] = mergeBetter(state.projects[cur], pchgs[i].mods);
          }
        } else if (pchgs[i].type === S.ChangeType.DELETE) {
          state.projects = state.projects.filter((proj) => proj.id !== pchgs[i].key);
        } else if (pchgs[i].type === S.ChangeType.CREATE) {
          state.projects.push(new S.Project(mergeBetter(pchgs[i].obj, {id: pchgs[i].key}) as S.ProjectStored));
        }
      }
      if (state.currentProject) {
        const currentProject = state.currentProject;
        // update the current project
        let pchg = pchgs.find((p: any) => currentProject.id === p.key);
        if (pchg !== undefined) {
          if (pchg.type === S.ChangeType.UPDATE) {
            state.currentProject = mergeBetter(state.currentProject, pchg.mods);
          } else if (pchg.type === S.ChangeType.DELETE) {
            state.currentProject = undefined;
          } else {
            throw new Error("Something bad happened when applying server changes to the current project.");
          }
        }
        if (currentProject.currentQuestion) {
          const question = currentProject.currentQuestion;
          // TODO: Do something to update the list of questions, runFile, openFile
          if (question.currentFile) {
            const file = question.currentFile;
            // update the current file
            let fchg = chgs.find((chg: any) => chg.table === "files"
              && chg.key === file.id);
            if (fchg !== undefined) {
              // if this file entry has been deleted, just jump out of the file
              if (fchg.type === S.ChangeType.DELETE) {
                showInfo("The current file was modified on the server. Open it again to edit it.");
                if (state.currentProject
                    && state.currentProject.currentQuestion) {
                  state.currentProject.currentQuestion.currentFile = undefined;
                }
              } else {
                throw new Error("Something bad happened when applying server changes to the current file.");
              }
            }
          }
        }
      }
      return state;
    default:
      return state;
  }
}
