import { connect } from "react-redux";
import { ComponentClass } from "react";
import { globalState } from "../reducers/";
import { projectRef, fileRef } from "../types";
import { appStateActions}  from "../reducers/appStateReducer";
import { userActions } from "../reducers/userReducer";
import { Services } from "../helpers/Services";
import { GenericError, LoginError } from "../helpers/Errors";
import { showError } from "../partials/Errors";
import { trim } from "ramda";
import { settingsActions, settingsReducerState } from "../reducers/settingsReducer";
import * as S from "../helpers/Storage/Interface";
import * as C from "../helpers/Compiler/Interface";


interface Func<T> {
  ([...args]: any): T;
}

function returnType<T>(func: Func<T>) {
  return null as T;
}

const mapStoreToProps = (state: globalState) => state;

const mapDispatchToProps = (dispatch: Function) => {

  async function asyncAction<T>(pr: Promise<T>) {
    try {
      const result = await pr;
      return result;
    } catch (e) {
      console.error(e);
      if (e instanceof LoginError) {
        showError(e.message);
        dispatch({type: userActions.INVALIDATE});
        throw null;
      } else {
        throw e;
      }
    }
  }

  const actions =  {
    dispatch: {
      settings: {
        updateSettings: (newSettings: settingsReducerState) => {
          asyncAction(Services.storage().setSettings({
            id: 0,
            editor_mode: "standard",
            font_size: newSettings.fontSize,
            font: newSettings.font,
            theme: newSettings.theme ? "light" : "dark",
            space_tab: true,
            tab_width: newSettings.tabWidth
          })).then(() => dispatch({
            type: settingsActions.updateSettings,
            payload: newSettings
          }));
        },
        updateEditorRatio: (ratio: number) => dispatch({
          type: settingsActions.updateEditorRatio,
          payload: ratio
        })
      },
      // other than openFile and closeFile, the file name parameter should always
      //  be the full path, for exmaple "q1/file.txt"
      file: {
        setFileOpTarget: (file: S.FileBrief) => dispatch({
          type: appStateActions.setFileOpTarget,
          payload: file
        }),
        invalidateFile: () => dispatch({
          type: appStateActions.invalidateFile,
          payload: {}
        }),
        copyFile: (targetName: string) => {
          // TODO: hook up to storage once we get a proper copy function
          dispatch({
            type: appStateActions.copyFile,
            payload: {
              question: {name: "question", files: ["file1.txt"]},
              newName: targetName.split("/").pop()
            }
          });
        },
        updateFile: (project: string, path: string, newFileContent: string) => {
          dispatch({type: appStateActions.changeFileBufferedContent, payload: {
            unwrittenContent: newFileContent,
            target: [project, path],
            flusher: () => {
              actions.dispatch.file.flushFileBuffer();
            }}});
        },
        flushFileBuffer: () => {
          return new Promise((resolve, reject) => {
            dispatch((dispatch: Function, getState: () => globalState) => {
              const {flusher, target, unwrittenContent} = getState().appState.currentProject.currentQuestion.currentFile;
              if (flusher) {
                clearTimeout(flusher);
              }
              if (target && unwrittenContent) {
                asyncAction(Services.storage().writeFile(target, unwrittenContent))
                  .then(() => dispatch({
                    type: appStateActions.changeFileContent,
                    payload: unwrittenContent,
                })).then(resolve).catch(reject);
              } else {
                resolve();
              }
            });
          });
        },
        switchFile: (file: S.FileBrief) => {
          return actions.dispatch.file.flushFileBuffer()
            .then(() => { return asyncAction(Services.storage().readFile(file.id)); })
            .then((file: S.File) => dispatch({
              type: appStateActions.switchFile,
              payload: file
            }));
        },
        addFile: (project: string, path: string,
                  newFileContent: string, currentQuestion: string) => {
          // writes a new file, returns a promise the caller can use when finished
          //  to do other stuff (i.e. switch to the file)
          return asyncAction(Services.storage().newFile(project, path, newFileContent))
            .then(() => dispatch({
              type: appStateActions.addFile,
              payload: path
            })).catch((reason) => {
              showError(reason);
            });
        },
        deleteFile: (file: S.FileBrief) => {
          return asyncAction(Services.storage().deleteFile(file.id))
            .then(() => {
              dispatch({
                type: appStateActions.closeFile,
                payload: file.name
              });
              dispatch({
                type: appStateActions.removeFile,
                payload: {name: file.name}
              });
            }).catch((reason) => {
              showError(reason);
            });
        },
        renameFile: (file: S.FileBrief, targetName: string) => {
          return asyncAction(Services.storage().renameFile(file.id, targetName))
            .then(() => {
              dispatch({
                type: appStateActions.closeFile,
                payload: file.name
              });
              dispatch({
                type: appStateActions.removeFile,
                payload: {name: file.name}
              });
              dispatch({
                type: appStateActions.addFile,
                payload: {name: targetName}
              });
            }).catch((reason) => {
              showError(reason);
            });
        },
        openFile: (file: S.FileBrief) => dispatch({
          type: appStateActions.openFile,
          payload: file
        }),
        closeFile: (file: S.FileBrief) => dispatch({
          type: appStateActions.closeFile,
          payload: file
        }),
        setRunFile: (file: S.FileBrief) => dispatch({
          type: appStateActions.setRunFile,
          payload: file
        })
      },
      question: {
        addQuestion: (newQuestionName: string) => dispatch({
          type: appStateActions.addQuestion,
          payload: {name: newQuestionName}
        }),
        removeQuestion: (name: string) => dispatch({
          type: appStateActions.removeQuestion,
          payload: {name: name}
        }),
        switchQuestion: (pid: S.ProjectID, name: string) => {
          return actions.dispatch.file.flushFileBuffer()
            .then(() => {
              return asyncAction(Services.storage().getProjectFiles(pid))
              .then((files: S.FileBrief[]) => dispatch({
                type: appStateActions.switchQuestion,
                payload: {
                  question: {
                    name: name,
                    runFile: "",
                    currentFile: {name: "", content: ""},
                    openFiles: [],
                    diags: [],
                    files: files.filter((file) => file.name.split("/")[0] === name)
                  }
                }
            }));
          });
        }
      },
      user: {
        signin: (username: string, password: string) => {
          return new Promise((resolve, reject) => {
            if (trim(username) === "" || trim(password) === "") {
              showError("Please fill in all fields!");
              reject(null);
            } else {
              asyncAction(Services.login(username, password)).then((response) => {
                dispatch({type: userActions.SIGNIN, payload: username});
                resolve();
              }).catch((e) => {
                reject(e);
              });
            }
          });
        },
        signout: () => {
          asyncAction(Services.logout()).then((response) => {
            dispatch({type: userActions.SIGNOUT});
          }).catch((reason) => {
            if (reason !== null) throw reason;
          });
        },
      },
      project: {
        addProject: (newProjectName: string) =>
          asyncAction(Services.storage().newProject(newProjectName)).then(() => dispatch({
            type: appStateActions.addProject,
            payload: {name: newProjectName}
        })),
        removeProject: (pid: S.ProjectID) =>
          asyncAction(Services.storage().deleteProject(name)).then(() => dispatch({
            type: appStateActions.removeProject,
            payload: {name: name}
        })),
        switchProject: (pid: S.ProjectID) => {
          // we will leave switching question and file to the UI
          // efficiency is for noobs
          function unique(val: any, idx: Number, arr: any) {
            return arr.indexOf(val) === idx;
          }
          return asyncAction(Services.storage().getProjectFiles(pid)
            .then((files: S.FileBrief[]) => dispatch({
              type: appStateActions.switchProject,
              payload: {
                project: {
                  termWrite: null,
                  termClear: null,
                  name: name,
                  id: pid,
                  questions: files.map((file) => file.name.split("/")[0]).filter(unique).reverse(),
                  currentQuestion: {
                    name: "",
                    files: [],
                    runFile: "",
                    openFiles: [],
                    diags: [],
                    currentFile: {
                      name: "",
                      content: ""
                    }
                  }
                }
              }
            })));
        },
        getAllProjects: () => {
          asyncAction(Services.storage().getProjects()).then((projects) => dispatch({
            type: appStateActions.getProjects,
            payload: {
              projects: projects.map((project) => project.name)
            }
          }));
        }
      },
      compile: {
        compileAndRun: (project: string, question: string, fid: S.FileID, test: boolean) => {
          dispatch({
            type: appStateActions.clearConsole,
            payload: {}
          });
          dispatch({
            type: appStateActions.setCompiling,
            payload: {}
          });
          asyncAction(Services.compiler().compileAndRunProject(project,
              question, fid, test)).then((result: C.CompilerResult) => {
            dispatch({
              type: appStateActions.setDiags,
              payload: result.messages
            });
            if (result.status !== "running") {
              dispatch({
                type: appStateActions.setNotRunning,
                payload: {}
              });
            } else {
              dispatch({
                type: appStateActions.setRunning,
                payload: {}
              });
            }
          }).catch((reason) => {
            dispatch({
              type: appStateActions.setNotRunning,
              payload: {}
            });
            if (reason !== null) {
              showError(reason.message);
            }
          });
        },
        setNotRunning: () => dispatch({
          type: appStateActions.setNotRunning,
          payload: {}
        }),
        stopProgram: () => Services.compiler().programKill().then(() => dispatch({
          type: appStateActions.setNotRunning,
          payload: {}
        })),
      },
      app: {
        setTerm: (termWrite: Function, termClear: Function) => dispatch({
          type: appStateActions.setTerm,
          payload: {
            termWrite: termWrite,
            termClear: termClear
          }
        }),
        writeConsole: (content: string) => dispatch({
          type: appStateActions.writeConsole,
          payload: {content: content}
        })
      }
    }
  };
  return actions;
};

const actionsStoreType = returnType(mapDispatchToProps);

export function getDispatch<actionsStoreType>(dispatch: Function) {
  return mapDispatchToProps(dispatch);
}

export type actionsInterface = typeof actionsStoreType & globalState;

export function map<PropertyType>(Component: ComponentClass<any>) {
  return connect<{}, {}, PropertyType>(mapStoreToProps, mapDispatchToProps)(Component);
}
