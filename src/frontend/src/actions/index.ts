import { connect } from "react-redux";
import { ComponentClass } from "react";
import { globalState } from "../reducers/";
import {projectRef, fileRef} from "../types";
import {appStateActions} from "../reducers/appStateReducer";
import {userActions} from "../reducers/userReducer";
import { Services } from "../helpers/Services";
import { GenericError, LoginError } from "../helpers/Errors";
import { showError } from "../partials/Errors";
import { trim } from "ramda";
import {settingsActions, settingsReducerState} from "../reducers/settingsReducer";
import {File} from "../helpers/Storage/Interface";


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
        }catch (e) {
            if (e instanceof LoginError) {
                showError(e.message);
                dispatch({type: userActions.INVALIDATE});
                throw null;
            } else {
                throw e;
            }
        }
    }

    return {
        dispatch: {
          settings: {
             updateSettings: (newSettings: settingsReducerState) => {
                asyncAction(Services.storage().setSettings({id: 0, editor_mode: "standard",
                font_size: newSettings.fontSize,
                font: newSettings.font,
                theme: newSettings.theme ? "light" : "dark",
                space_tab: true,
                tab_width: newSettings.tabWidth})).then(() => dispatch({type: settingsActions.updateSettings, payload: newSettings}));
                },
             updateEditorRatio: (ratio: number) => dispatch({type: settingsActions.updateEditorRatio, payload: ratio})
          },
          // other than switchfile, openFile and closeFile, the file name parameter should always be the full path, for exmaple "q1/file.txt"
          file: {
              copyFile: (targetName: string) => {
                  // TODO: hook up to storage once we get a proper copy function
                  dispatch({type: appStateActions.copyFile, payload: {question: {name: "question", files: ["file1.txt"]}, newName: targetName.split("/").pop()}})},
              updateFile: (project: string, path: string, newFileContent: string) => {
                  asyncAction(Services.storage().writeFile([project, path], newFileContent)).then(() => dispatch({type: appStateActions.changeFileContent, payload: newFileContent})); },
              addFile: (project: string, path: string, newFileContent: string, currentQuestion: string) => {
                  // writes a new file, returns a promise the caller can use when finished to do other stuff (i.e. switch to the file)
                  return asyncAction(Services.storage().newFile(project, path, newFileContent)).then(()=>dispatch({type: appStateActions.addFile, payload: path})).catch((reason)=>{throw reason;});},
              deleteFile: (project: string, name: string) => {
                  return asyncAction(Services.storage().deleteFile([project, name])).then(()=>{
                      dispatch({type: appStateActions.closeFile, payload: name});
                      dispatch({type: appStateActions.removeFile, payload: {name: name}});}).catch((reason)=>{throw reason;}); },
              renameFile: (project: string, currentname: string, targetName: string) => {
                  return asyncAction(Services.storage().renameFile([project, currentname], targetName)).then(()=>{
                      dispatch({type: appStateActions.closeFile, payload: name});
                      dispatch({type: appStateActions.removeFile, payload: name});
                      dispatch({type: appStateActions.addFile, payload: targetName});
                      dispatch({type: appStateActions.openFile, payload: targetName});
                  }).catch((reason)=>{throw reason;}); },
              openFile: (name: string) => dispatch({type: appStateActions.openFile, payload: name}),
              closeFile: (name: string) => dispatch({type: appStateActions.closeFile, payload: name}),
              setRunFile: (name: string) => dispatch({type: appStateActions.setRunFile, payload: name}),
              switchFile: (question: string, name: string) => {
                    return asyncAction(Services.storage().readFile([question, name])).then((file: File) => dispatch({type: appStateActions.switchFile, payload: {file: {name: name, content: file.contents}}}));
                }
          },
          question: {
              addQuestion: (newQuestionName: string) => dispatch({type: appStateActions.addQuestion, payload: {name: newQuestionName}}),
              removeQuestion: (name: string) => dispatch({type: appStateActions.removeQuestion, payload: {name: name}}),
              switchQuestion: (project: string, name: string) => {
                  return asyncAction(Services.storage().getFiles(project)).then((files) => dispatch({type: appStateActions.switchQuestion, payload: {question: {name: name, openFiles: [], files: files.filter((file) => file.name.split("/")[0] === name).map((file) => file.name)}}}));
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
              addProject: (newProjectName: string) => asyncAction(Services.storage().newProject(newProjectName)).then(() => dispatch({type: appStateActions.addProject, payload: {name: newProjectName}})),
              removeProject: (name: string) => asyncAction(Services.storage().deleteProject(name)).then(() => dispatch({type: appStateActions.removeProject, payload: {name: name}})),
              switchProject: (name: string) => {
                  // TODO interface with backend and get real data
                  // we will leave switching question and file to the UI
                  // efficiency is for noobs
                  function unique(val: any, idx: Number, arr: any){
                    return arr.indexOf(val) === idx;
                  }
                  return asyncAction(Services.storage().getFiles(name)).then((files) => Services.storage().getProject(name).then((project)=>dispatch({type: appStateActions.switchProject, payload: {project: {name: name, id: project.id, questions: files.map((file) => file.name).filter(unique),
                    currentQuestion: {name: "", files: [], runFile: "", openFiles: [], currentFile: {name: "", connect: ""}}}}})));
              },
              getAllProjects: ()=> {
                  asyncAction(Services.storage().getProjects()).then((projects)=>dispatch({type: appStateActions.getProjects, payload: {projects: projects.map((project)=>project.name)}}));
              }
          },
          app: {
              // removeError: (errorIDX: Number) => dispatch({type: appStateActions.removeError, payload: {errorIDX: errorIDX}})

          }
        }
    };
};

const actionsStoreType = returnType(mapDispatchToProps);


export function getDispatch<actionsStoreType>(dispatch: Function) {
    return mapDispatchToProps(dispatch);
}

export type actionsInterface = typeof actionsStoreType & globalState;

export function map<PropertyType>(Component: ComponentClass<any>) {
    return connect<{}, {}, PropertyType>(mapStoreToProps, mapDispatchToProps)(Component);
}
