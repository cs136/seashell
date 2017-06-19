import { connect } from "react-redux";
import { ComponentClass } from "react";
import { globalState } from "../reducers/";
import { projectRef, fileRef } from "../types";
import { appStateActions } from "../reducers/appStateReducer";
import { userActions } from "../reducers/userReducer";
import { Services } from "../helpers/Services";
import { Settings } from "../helpers/Storage/Interface";
import { GenericError, LoginError } from "../helpers/Errors";
import { showError } from "../partials/Errors";
import { trim } from "ramda";
import { settingsActions, settingsReducerState, settingsReducerStateNullable } from "../reducers/settingsReducer";
import * as S from "../helpers/Storage/Interface";
import * as C from "../helpers/Compiler/Interface";
import { dialogActions } from "../reducers/dialogReducer";


interface Func<T> {
  ([...args]: any): T;
}

function returnType<T>(func: Func<T>) {
  return (false as true) && func([]);
}

const mapStoreToProps = (state: globalState) => state;

const mapDispatchToProps = (dispatch: Function) => {

  async function asyncAction<T>(pr: Promise<T>) {
    try {
      const result = await pr;
      return result;
    } catch (e) {
      console.error(e);
      if (e.message) {
        showError(e.message);
      }
      if (e instanceof LoginError) {
        dispatch({ type: userActions.INVALIDATE });
        throw null;
      } else {
        throw e;
      }
    }
  }

  const storage = Services.getStorage;
  const webStorage = Services.getWebStorage;

  const actions =  {
    dispatch: {
      dialog: {
        setReset: (val: boolean) => {
          if (val) {
            dispatch({ type: dialogActions.open, payload: "reset" });
          }
          else {
            dispatch({ type: dialogActions.close, payload: "reset" });
          }
        },
        toggleResetOpen: () => {
          dispatch({ type: dialogActions.toggle, payload: "reset_open" });
        },
        toggleHelp: () => {
          dispatch({ type: dialogActions.toggle, payload: "help_open" });
        },
        toggleSettings: () => {
          dispatch({ type: dialogActions.toggle, payload: "settings_open" });
        },
        toggleAddProject: () => {
          dispatch({ type: dialogActions.toggle, payload: "add_project_open" });
        },
        toggleDeleteFile: () => {
          dispatch({ type: dialogActions.toggle, payload: "delete_file_open" });
        },
        toggleDeleteProject: () => {
          dispatch({ type: dialogActions.toggle, payload: "delete_project_open" });
        },
        toggleRenameFile: () => {
          dispatch({ type: dialogActions.toggle, payload: "rename_file_open" });
        },
        toggleCopyFile: () => {
          dispatch({ type: dialogActions.toggle, payload: "copy_file_open" });
        },
        toggleAddFile: () => {
          dispatch({ type: dialogActions.toggle, payload: "add_file_open" });
        },
        toggleAddTest: () => {
          dispatch({ type: dialogActions.toggle, payload: "add_test_open" });
        }
      },
      settings: {
        initSettings: () => {
          asyncAction(storage().getSettings()).then((settings) => {
            dispatch({
              type: settingsActions.updateSettings,
              payload: {
                font: settings.font,
                fontSize: settings.font_size,
                editorMode: 0,
                tabWidth: settings.tab_width,
                theme: settings.theme === "light" ? 1 : 0,
                offlineMode: Services.getOfflineMode(),
                editorRatio: 0.5,
                updated: 0,
              }
            });
          });
        },
        updateSettings: (newSettings: settingsReducerStateNullable) => {
          dispatch((dispatch: Function, getState: () => globalState) => {
            let oldSettings = getState().settings;
            dispatch({
              type: settingsActions.updateSettings,
              payload: newSettings
            });
            dispatch((dispatch: Function, getState: () => globalState) => {
              let newSettings = getState().settings;
              asyncAction(storage().setSettings(Settings.fromJSON({
                id: 0,
                editor_mode: "standard",
                font_size: newSettings.fontSize,
                font: newSettings.font,
                theme: newSettings.theme ? "light" : "dark",
                space_tab: true,
                tab_width: newSettings.tabWidth,
              })));
            });
            if (newSettings.offlineMode !== undefined) {
              Services.setOfflineMode(newSettings.offlineMode);
              // Force reload
              if (newSettings.offlineMode === 0 &&
                  oldSettings.offlineMode > 0 ||
                  newSettings.offlineMode > 0 &&
                  oldSettings.offlineMode === 0) {
                    window.location.hash = "";
                    window.location.reload(true);
                  }
            }
          });
        },
        updateEditorRatio: (ratio: number) => dispatch({
          type: settingsActions.updateEditorRatio,
          payload: ratio
        })
      },
      // other than openFile and closeFile, the file name parameter should always
      //  be the full path, for example "q1/file.txt"
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
          // TODO: hook up to storage() once we get a proper copy function
          dispatch({
            type: appStateActions.copyFile,
            payload: {
              question: { name: "question", files: ["file1.txt"] },
              newName: targetName.split("/").pop()
            }
          });
          return Promise.resolve();
        },
        updateFile: (file: S.File, newFileContent: string) => {
          dispatch({
            type: appStateActions.changeFileBufferedContent,
            payload: {
              unwrittenContent: newFileContent,
              target: file.id,
              flusher: () => {
                actions.dispatch.file.flushFileBuffer();
              }
            }
          });
        },
        flushFileBuffer: () : Promise<void> => {
          return new Promise<void>((resolve, reject) => {
            dispatch((dispatch: Function, getState: () => globalState) => {
              const state = getState();
              if (state.appState.currentProject &&
                state.appState.currentProject &&
                state.appState.currentProject.currentQuestion &&
                state.appState.currentProject.currentQuestion.currentFile) {
                const { flusher, target, unwrittenContent } = state.appState.currentProject.currentQuestion.currentFile;
                if (flusher) {
                  clearTimeout(flusher);
                }
                if (target && unwrittenContent) {
                  asyncAction(storage().writeFile(target, unwrittenContent))
                    .then(() => dispatch({
                      type: appStateActions.changeFileContent,
                      payload: unwrittenContent,
                    })).then(resolve).catch(reject);
                } else {
                  resolve();
                }
              } else {
                resolve(); // Nothing to flush
              }
            });
          });
        },
        switchFile: (file: S.FileBrief) => {
          console.log("switchfile-action");
          return actions.dispatch.file.flushFileBuffer()
            .then(() => { return asyncAction(storage().readFile(file.id)); })
            .then((file: S.File) => dispatch({
              type: appStateActions.switchFile,
              payload: file
            }));
        },
        addFile: (project: string, question: string, path: string, newFileContent: string) => {
          // writes a new file, returns a promise the caller can use when finished
          //  to do other stuff (i.e. switch to the file)
          return asyncAction(storage().newFile(project, path, newFileContent))
            .then((file) => {
              dispatch({
                type: appStateActions.addFile,
                payload: file
              });
              return asyncAction(storage().addOpenFile(file.project, question, file.id))
                .then(async () => {
                  // file needs to be read here to obtain the default contents
                  file = await storage().readFile(file.id);
                  dispatch({
                    type: appStateActions.openFile,
                    payload: file
                  });
                  dispatch({
                    type: appStateActions.switchFile,
                    payload: file
                  });
                });
            });
        },
        deleteFile: (file: S.FileBrief) => {
          return asyncAction(storage().deleteFile(file.id))
            .then(() => {
              dispatch({
                type: appStateActions.closeFile,
                payload: file
              });
              dispatch({
                type: appStateActions.removeFile,
                payload: file
              });
              return asyncAction(webStorage().pullMissingSkeletonFiles(file.project));
            });
        },
        renameFile: (file: S.FileBrief, targetName: string) => {
          return asyncAction(storage().renameFile(file.id, targetName))
            .then((newFile) => {
              dispatch({
                type: appStateActions.closeFile,
                payload: file
              });
              dispatch({
                type: appStateActions.removeFile,
                payload: file
              });
              dispatch({
                type: appStateActions.addFile,
                payload: newFile
              });
              dispatch({
                type: appStateActions.updateCurrentFileIfIdEquals,
                payload: {oldFid: file.id, newFileBrief: newFile}
              });
              return asyncAction(webStorage().pullMissingSkeletonFiles(file.project))
                .then(() => newFile);
            });
        },
        openFile: (question: string, file: S.FileBrief) => {
          storage().addOpenFile(file.project, question, file.id).then((questions) =>
            dispatch({
              type: appStateActions.openFile,
              payload: file
            }));
        },
        closeFile: (file: S.FileBrief) => {
          storage().removeOpenFile(file.project, file.question(), file.id).then((questions) =>
            dispatch({
              type: appStateActions.closeFile,
              payload: file
            }));
        },
        setRunFile: (question: string, file: S.FileBrief) => {
          storage().setFileToRun(file.project, question, file.name).then(() => dispatch({
            type: appStateActions.setRunFile,
            payload: file.name
          }));
        },
      },
      question: {
        addQuestion: (newQuestionName: string) => dispatch({
          type: appStateActions.addQuestion,
          payload: { name: newQuestionName }
        }),
        removeQuestion: (name: string) => dispatch({
          type: appStateActions.removeQuestion,
          payload: { name: name }
        }),
        switchQuestion: (pid: S.ProjectID, name: string) => {
          return actions.dispatch.file.flushFileBuffer()
            .then(() => {
              return asyncAction(storage().getProjectFiles(pid))
                .then((files: S.FileBrief[]) => {
                  return asyncAction(storage().getOpenFiles(pid, name))
                    .then((openFiles) => {
                      return asyncAction(storage().getFileToRun(pid, name)
                        .then((runFile) => {
                          const question = {
                            name: name,
                            runFile: runFile,
                            currentFile: undefined,
                            openFiles: openFiles.filter((file) => file.question() === name ||
                              file.question() === "common"),
                            diags: [],
                            files: files.filter((file) => file.question() === name ||
                              file.question() === "common")
                          };
                          dispatch({
                            type: appStateActions.switchQuestion,
                            payload: {
                              question: question
                            }
                          });
                          return question;
                        }));
                    });
                });
            });
        },
        getMarmosetResults: async (projectName: string, questionName: string) => {
          const oldLength = JSON.parse((await asyncAction(webStorage().getTestResults(projectName + questionName)))).result.length;
          await asyncAction(webStorage().marmosetSubmit(projectName, projectName + questionName, questionName));
          let result = [];
          function sleep(milliseconds: Number) {
            return new Promise(resolve => setTimeout(resolve, milliseconds));
          }
          while (result.length === oldLength || result.length === 0 || result[0].status !== "complete") {
            const response = (await asyncAction(webStorage().getTestResults(projectName + questionName)));
            result = JSON.parse(response).result;
            if (result.length === oldLength || result.length === 0 || result[0].status !== "complete") await sleep(1500); // let's not destroy the server
          }
          console.log(result[0]);
          return result[0];
        }
      },
      user: {
        signin: (username: string, password: string, reset: boolean) => {
          dispatch({ type: userActions.BUSY });
          return new Promise((resolve, reject) => {
            if (trim(username) === "" || trim(password) === "") {
              dispatch({ type: userActions.NOTBUSY });
              showError("Please fill in all fields!");
              reject(null);
            } else {
              const path = window.location.pathname.substring(0, window.location.pathname.lastIndexOf("/"));
              asyncAction(Services.login(username,
                                         password,
                                         reset,
                                         PRODUCTION ? `https://${window.location.host}${path}/cgi-bin/login2.cgi` : undefined))
              .then((response) => {
                dispatch({ type: userActions.SIGNIN, payload: username });
                resolve();
              }).catch((e) => {
                dispatch({ type: userActions.NOTBUSY });
                reject(e);
              });
            }
          });
        },
        signout: () => {
          asyncAction(Services.logout()).then((response) => {
            dispatch({ type: userActions.SIGNOUT });
          }).catch((reason) => {
            if (reason !== null) throw reason;
          });
        },
        autoConnect: async () => {
          // We don't use asyncAction here as we're suppressing
          // any errors that happen when auto connecting --
          // we're in the login screen, let user manually log in
          // if we can't auto login.
          // dispatch({ type: userActions.BUSY });
          try {
            let user = await Services.autoConnect();
            dispatch({type: userActions.BUSY});
            dispatch({ type: userActions.SIGNIN, payload: user });
            return user;
          } catch (e) {
            dispatch({ type: userActions.NOTBUSY });
            throw e;
          }
        }
      },
      project: {
        addProject: (newProjectName: string) => {
          return asyncAction(storage().newProject(newProjectName)).then((proj: S.ProjectBrief) => {
            dispatch({
              type: appStateActions.addProject,
              payload: proj
            });
            return proj;
          });
        },
        removeProject: (pid: S.ProjectID) => {
          return asyncAction(webStorage().inSkeleton(pid)).then((inSkel) => {
            if (inSkel) {
              showError("This project was provided by the course staff and cannot be deleted.");
            } else {
              return asyncAction(storage().deleteProject(pid)).then(() => dispatch({
                type: appStateActions.removeProject,
                payload: { id: pid }
              }));
            }
          });
        },
        switchProject: (name: string, pid: S.ProjectID) => {
          // we will leave switching question and file to the UI
          // efficiency is for noobs
          function uniqueNotCommon(val: string, idx: Number, arr: any) {
            return arr.indexOf(val) === idx && val !== "common";
          }
          dispatch({type: appStateActions.switchProject, payload: {project: null}});
          return asyncAction(webStorage().pullMissingSkeletonFiles(pid)).then(() => {
            return asyncAction(storage().getProjectFiles(pid))
              .then((files: S.FileBrief[]) => dispatch({
                type: appStateActions.switchProject,
                payload: {
                  project: {
                    termWrite: null,
                    termClear: null,
                    name: name,
                    id: pid,
                    questions: files.map((file) => file.question()).filter(uniqueNotCommon),
                    currentQuestion: undefined
                  }
                }
              }));
          });
        },
        getAllProjects: () => {
          return asyncAction(webStorage().fetchNewSkeletons()).then(() => {
            return asyncAction(storage().getProjects()).then((projects) => dispatch({
              type: appStateActions.getProjects,
              payload: {
                projects: projects
              }
            }));
          });
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
          asyncAction(actions.dispatch.file.flushFileBuffer())
            .then(() =>
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
                    }));
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
