import { connect } from "react-redux";
import { ComponentClass } from "react";
import { globalState } from "../reducers/";
import { appStateActions } from "../reducers/appStateReducer";
import { userActions } from "../reducers/userReducer";
import { Services } from "../helpers/Services";
import { Settings } from "../helpers/Storage/Interface";
import { GenericError, LoginError, ConflictError } from "../helpers/Errors";
import { showError } from "../partials/Errors";
import { trim } from "ramda";
import { settingsActions, settingsReducerState, settingsReducerStateNullable } from "../reducers/settingsReducer";
import * as S from "../helpers/Storage/Interface";
import * as C from "../helpers/Compiler/Interface";
import { dialogActions } from "../reducers/dialogReducer";
import { saveAs } from "file-saver";
import JSZip = require("jszip");
import * as Raven from "raven-js";

interface Func<T> {
  ([...args]: any): T;
}

function returnType<T>(func: Func<T>) {
  return (false as true) && func([]);
}

const mapStoreToProps = (state: globalState) => state;

const mapDispatchToProps = (dispatch: Function) => {

  async function asyncAction<T>(pr: Promise<T>): Promise<T> {
    try {
      const result = await pr;
      return result;
    } catch (e) {
      // in case of a conflict, override regular error handling
      if (e instanceof ConflictError) {
        dispatch({
          type: appStateActions.conflictOccurred,
          payload: e.results
        });
        await actions.dispatch.dialog.toggleResolveConflict();
        throw e;
      }
      // if it's a login error, just redirect to the login screen
      else if (e instanceof LoginError) {
        dispatch({ type: userActions.INVALIDATE });
        throw null;
      } else {
        // Suppress handling null (LoginError) messages
        if (e) {
          // Display the error
          if (typeof e === "object" && e.message && !e._asyncAction_shown) {
            showError(e.message);
          }
          // Log and Report exception to Sentry, if not shown
          if (typeof e === "object" && !e._asyncAction_shown) {
            Raven.captureException(e);
            console.error(e);
          }
          // Wrap e in a Error if it's not an object
          if (typeof e !== "object") {
            e = new Error(e);
            (<any>e)._asyncAction_shown = true;
          }
          // Rethrow error (with _asyncAction_shown set to true so we don't log the error twice)
          throw e;
        } else {
          throw e;
        }
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
        },
        toggleAddQuestion: () => {
          dispatch({ type: dialogActions.toggle, payload: "add_question_open" });
        },
        toggleResolveConflict: () => {
          dispatch({ type: dialogActions.toggle, payload: "resolve_conflict_open" });
        },
        toggleArchive: () => {
          dispatch({ type: dialogActions.toggle, payload: "archive_open" });
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
          });
        },
        updateEditorRatio: (ratio: number) => dispatch({
          type: settingsActions.updateEditorRatio,
          payload: ratio
        }),
        adjustFont: (toAdd: number) => dispatch({
          type: settingsActions.adjustFont,
          payload: toAdd
        })
      },
      // other than openFile and closeFile, the file name parameter should always
      //  be the full path, for example "q1/file.txt"
      file: {
        setFileOpTarget: (filename: string) => dispatch({
          type: appStateActions.setFileOpTarget,
          payload: filename
        }),
        invalidateFile: () => dispatch({
          type: appStateActions.invalidateFile,
          payload: {}
        }),
        copyFile: async (project: S.ProjectID, question: string, source: string, target: string) => {
          let file = await asyncAction(storage().getFileByName(project, source));
          if (file && file.contents) {
            return actions.dispatch.file.addFile(project, question, target, file.contents.contents);
          }
          else {
            let error = new Error(`Can not copy file ${source} as it has no contents!`);
            return asyncAction(Promise.reject(error));
          }
        },
        updateFile: (file: S.FileEntry, newFileContent: string) => {
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
        flushFileBuffer: () : Promise<boolean> => {
          return new Promise<boolean>((resolve, reject) => {
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
                    .then((fid: S.FileID) => {
                      dispatch({
                        type: appStateActions.changeFileContent,
                        payload: {
                          contents: unwrittenContent,
                          id: fid
                        }
                      });
                      resolve(true);
                    }).catch(reject);
                } else {
                  resolve(false);
                }
              } else {
                resolve(false); // Nothing to flush
              }
            });
          });
        },
        switchFile: (project: S.ProjectID, filename: string) => {
          return actions.dispatch.file.flushFileBuffer()
            .then(() => {
              return asyncAction(storage().getFileByName(project, filename)).then((fullfile) => {
                return asyncAction(storage().getVersions(project, filename)).then((versions) => {
                  if (fullfile) {
                    dispatch({
                      type: appStateActions.switchFile,
                      payload: {
                        file: fullfile,
                        versions: versions,
                        project: project
                      }
                    });
                  }
                });
              });
            });
        },
        addFile: (project: S.ProjectID, question: string, filename: string, newFileContent: string) => {
          return asyncAction(storage().newFile(project, filename, newFileContent))
            .then((file) => {
              dispatch({
                type: appStateActions.addFile,
                payload: {
                  file: filename,
                  project: project,
                  question: question
                }
              });
              return asyncAction(storage().addOpenFile(file.project_id, question, file.name))
                .then(async () => {
                  let entry = await asyncAction(storage().getFileByName(file.project_id, file.name));
                  dispatch({
                    type: appStateActions.openFile,
                    payload: {
                      file: entry.name,
                      project: project,
                      question: question
                    }
                  });
                  dispatch({
                    type: appStateActions.switchFile,
                    payload: {
                      file: entry,
                      project: project,
                      question: question
                    }
                  });
                });
            });
        },
        deleteFile: (project: S.ProjectID, question: string, filename: string) => {
          return asyncAction(storage().deleteFile(project, filename))
            .then(async () => {
              await storage().removeOpenFile(project, question, filename);
              dispatch({
                type: appStateActions.closeFile,
                payload: {
                  file: filename,
                  project: project,
                  question: question
                }
              });
              dispatch({
                type: appStateActions.removeFile,
                payload: {
                  file: filename,
                  project: project,
                  question: question
                }
              });
              try {
                return await asyncAction(webStorage().pullMissingSkeletonFiles(project));
              } catch (e) {
                console.warn("Could not load missing skeleton files after deleting %s -- %s", filename, e);
              }
            });
        },
        renameFile: (project: S.ProjectID, question: string, currentName: string, targetName: string) => {
          return asyncAction(storage().renameFile(project, currentName, targetName))
            .then(async (newFile) => {
              dispatch({
                type: appStateActions.closeFile,
                payload: {
                  file: currentName,
                  project: project,
                  question: question
                }
              });
              dispatch({
                type: appStateActions.removeFile,
                payload: {
                  file: currentName,
                  project: project,
                  question: question
                }
              });
              dispatch({
                type: appStateActions.addFile,
                payload: {
                  file: targetName,
                  project: project,
                  question: question
                }
              });
              dispatch({
                type: appStateActions.updateCurrentFileIfNameEquals,
                payload: {
                  oldName: currentName,
                  newFile: newFile,
                  project: project,
                  question: question
                }
              });
              await storage().removeOpenFile(project, question, currentName);
              return asyncAction(webStorage().pullMissingSkeletonFiles(project))
                .then(() => newFile)
                .catch((e) => newFile); // ignore skeleton failures
            });
        },
        openFile: (project: S.ProjectID, question: string, filename: string) => {
          return asyncAction(storage().addOpenFile(project, question, filename)).then((questions) =>
            dispatch({
              type: appStateActions.openFile,
              payload: {
                file: filename,
                project: project,
                question: question
              }
            }));
        },
        closeFile: (project: S.ProjectID, question: string, filename: string) => {
          return asyncAction(storage().removeOpenFile(project, question, filename)).then((questions) =>
            dispatch({
              type: appStateActions.closeFile,
              payload: {
                file: filename,
                project: project,
                question: question
              }
            }));
        },
        setRunFile: (project: S.ProjectID, question: string, filename: string) => {
          return asyncAction(storage().setFileToRun(project, question, filename)).then(() => dispatch({
            type: appStateActions.setRunFile,
            payload: {
              file: filename,
              project: project,
              question: question
            }
          }));
        },
        revertFile: (fid: S.FileID, cnts: S.Contents) => {
          return asyncAction(storage().writeFile(fid, cnts.contents)).then((nid) => {
            dispatch({
              type: appStateActions.changeFileContent,
              payload: {
                contents: cnts.contents,
                id: nid
              }
            });
          });
        },
        resolveConflict: (contents: S.Contents) => {
          return asyncAction(storage().resolveConflict(contents)).then(() =>
            dispatch({
              type: appStateActions.conflictResolved,
              payload: null
            })
          );
        }
      },
      question: {
        addQuestion: (pid: S.ProjectID, newQuestionName: string) => {
          return asyncAction(storage().newQuestion(pid, newQuestionName)).then(() => {
            dispatch({
              type: appStateActions.addQuestion,
              payload: {
                name: newQuestionName,
                project: pid
              }
            });
          });
        },
        removeQuestion: (pid: S.ProjectID, name: string) => {
          return asyncAction(storage().deleteQuestion(pid, name)).then(() => {
            dispatch({
              type: appStateActions.removeQuestion,
              payload: {
                name: name,
                project: pid
              }
            });
          });
        },
        switchQuestion: (pid: S.ProjectID, name: string) => {
          return actions.dispatch.file.flushFileBuffer()
            .then(() => {
              return asyncAction(storage().getFiles(pid, name))
                .then((files: S.File[]) => {
                  return asyncAction(storage().getOpenFiles(pid, name))
                    .then((openFiles) => {
                      return asyncAction(storage().getFileToRun(pid, name)
                        .then((runFile) => {
                          const question = {
                            name: name,
                            runFile: runFile,
                            currentFile: undefined,
                            openFiles: openFiles,
                            diags: [],
                            files: files.map((file: S.File) => file.name)
                          };
                          dispatch({
                            type: appStateActions.switchQuestion,
                            payload: {
                              question: question,
                              project: pid
                            }
                          });
                          return question;
                        }));
                    });
                });
            });
        },
        marmosetSubmit: (project: S.ProjectID, question: string, marmosetProject: string) => {
          return asyncAction(actions.dispatch.file.flushFileBuffer())
                 .then(() => asyncAction(webStorage().marmosetSubmit(project,
                        marmosetProject, question)));
        },
        getMarmosetResults: async (marmosetProject: string) => {
          const oldLength = JSON.parse(await asyncAction(webStorage().getTestResults(marmosetProject))).result.length;
          let result: any[] = [];
          return new Promise<any>((accept, reject) => {
            dispatch((dispatch: Function, getState: () => globalState) =>
              dispatch({
                type: appStateActions.setMarmosetInterval,
                payload: setInterval(async () => {
                  if (result.length === oldLength || result.length === 0 || result[0].status !== "complete") {
                    const response = (await asyncAction(webStorage().getTestResults(marmosetProject)));
                    result = JSON.parse(response).result;
                    if (!(result.length === oldLength || result.length === 0 || result[0].status !== "complete")) {
                      clearInterval(getState().appState.marmosetInterval);
                      accept(result[0]);
                    }
                  }
                }, 4000)
              })
            );
          });
        },
        clearMarmosetInterval: () => {
          dispatch((dispatch: Function, getState: () => globalState) =>
            clearInterval(getState().appState.marmosetInterval)
          );
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
            await Services.autoConnect();
            let user = Services.session().username;
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
        downloadAll: () => {
          return asyncAction(storage().exportAsZip()).then(async (zip: JSZip) => {
            let blob = await zip.generateAsync({ type: "blob", compression: "DEFLATE" });
            saveAs(blob, Services.session().username + ".zip");
          });
        },
        downloadProject: (pid: S.ProjectID, name: string) => {
          return asyncAction(storage().exportAsZip(pid)).then(async (zip: JSZip) => {
            let blob = await zip.generateAsync({ type: "blob", compression: "DEFLATE" });
            saveAs(blob, name + ".zip");
          });
        },
        addProject: (newProjectName: string) => {
          return asyncAction(storage().newProject(newProjectName)).then((proj: S.Project) => {
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
        switchProject: async (name: string, pid: S.ProjectID) => {
          dispatch({type: appStateActions.switchProject, payload: {project: null}});
          try {
            await asyncAction(webStorage().pullMissingSkeletonFiles(pid));
          } catch (e) {
            console.warn("Could not load missing skeleton files for %s -- %s", name, e);
          }
          return asyncAction(storage().getQuestions(pid))
            .then((questions: string[]) => {
              dispatch({
                type: appStateActions.switchProject,
                payload: {
                  project: {
                    termWrite: null,
                    termClear: null,
                    name: name,
                    id: pid,
                    questions: questions,
                    currentQuestion: undefined
                  }
                }
              });
              return storage().updateLastUsed(pid);
            }).catch((e) => {
              console.warn(e);
              // If we fail to open the project, redirect home so we don't get stuck
              //  in a bad route
              dispatch({
                type: appStateActions.redirectHome,
                payload: null
              });
            });
        },
        getAllProjects: async () => {
          try {
            await asyncAction(webStorage().fetchNewSkeletons());
          } catch (e) { } // still want to get the projects if skeletons fail
          return asyncAction(storage().getProjects()).then((projects) => dispatch({
            type: appStateActions.getProjects,
            payload: {
              projects: projects
            }
          }));
        },
        getMarmosetProjects: () => {
          return asyncAction(webStorage().getMarmosetProjects()).then((lst) => dispatch({
            type: appStateActions.setMarmosetProjects,
            payload: lst
          }));
        },
        archiveProjects: () => {
          return asyncAction(webStorage().archiveProjects()).then(() =>
            dispatch({
              type: appStateActions.redirectHome,
              payload: null
            }));
        },
        makeConsistent: () => {
          dispatch({
            type: appStateActions.makeConsistent,
            payload: null
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
            .then((expectingChange) =>
                asyncAction(storage().waitForSync()).then(() =>
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
                    })).catch((reason) => {
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
