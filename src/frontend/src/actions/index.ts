import { connect } from "react-redux";
import { ComponentClass } from "react";
import { globalState } from "../reducers/";
import {projectRef, fileRef} from "../types";
import {appStateActions} from "../reducers/appStateReducer";
import {userActions} from "../reducers/userReducer";
import { Services, GenericError, LoginError } from "../helpers/Services";
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
            return await pr;
        }catch (e) {
            if (e instanceof LoginError) {
                showError(e.message);
                dispatch({type: userActions.INVALIDATE});
            } else {
                console.log(typeof e);
                throw e;
            }
        }
    }

    return {
        dispatch: {
          settings: {
             updateSettings: (newSettings: settingsReducerState) => {
                Services.storage().setSettings({id: 0, editor_mode: "standard",
                font_size: newSettings.fontSize,
                font: newSettings.font,
                theme: newSettings.theme ? "light" : "dark",
                space_tab: true,
                tab_width: newSettings.tabWidth}).then(() => dispatch({type: settingsActions.updateSettings, payload: newSettings}));
                },
             updateEditorRatio: (ratio: number) => dispatch({type: settingsActions.updateEditorRatio, payload: ratio})
          },
          // other than openFile and closeFile, the file name paramter should always be the full path, for exmaple "q1/file.txt"
          file: {
              copyFile: (targetName: string) => dispatch({type: appStateActions.copyFile, payload: {question: {name: "question", files: ["file1.txt"]}, newName: targetName.split("/").pop()}}),
              updateFile: (project: string, path: string, newFileContent: string) => {
                  Services.storage().writeFile([project, path], newFileContent).then(() => dispatch({type: appStateActions.changeFileContent, payload: newFileContent})); },
              addFile: (project: string, path: string, newFileContent: string) => {
                  // writes a new file, then opens the question the new file was added to and opens the newly added file
                  Services.storage().writeFile([project, path], newFileContent).then(()=>
                  Services.storage().getFiles(project).then((files)=>dispatch({type: appStateActions.addFile, payload: {name: path.split("/")[0],
                  files: files.filter((file)=>file.name.split("/")[0]===path.split("/")[0] || file.name.split("/")[0] === "tests" || file.name.split("/")[0] === "common").map((file) => file.name)},
                  file: {name: path.split("/")[0], content: newFileContent}}))); },
              deleteFile: (project: string, name: string) => {
                  Services.storage().deleteFile([project, name]).then(dispatch({type: appStateActions.removeFile, payload: {name: name}})); },
              renameFile: (project: string, currentname: string, targetName: string) => {
                  Services.storage().renameFile([project, currentname], targetName).then(() => Services.storage().getFiles(project).then((files) => dispatch({type: appStateActions.renameFile, payload: {question: {name: targetName.split("/")[0],
                  files: files.filter((file) => file.name.split("/")[0] === targetName.split("/")[0] || file.name.split("/")[0] === "tests" || file.name.split("/")[0] === "common").map((file) => file.name)},
                  newName: targetName.split("/").pop()}}))); },
              openFile: (name: string) => dispatch({type: appStateActions.openFile, payload: name}),
              closeFile: (name: string) => dispatch({type: appStateActions.closeFile, payload: name}),
              setRunFile: (name: string) => dispatch({type: appStateActions.setRunFile, payload: name}),
              switchFile: (question: string, name: string) => {
                    Services.storage().readFile([question, name]).then((file: File) => dispatch({type: appStateActions.switchFile, payload: {file: {name: name, content: file.contents}}}));
                }
          },
          question: {
              addQuestion: (newQuestionName: string) => dispatch({type: appStateActions.addQuestion, payload: {name: newQuestionName}}),
              removeQuestion: (name: string) => dispatch({type: appStateActions.removeQuestion, payload: {name: name}}),
              switchQuestion: (project: string, name: string) => {
                  // TODO interface with backend and get real data
                  // we will leave switching files to the UI
                  Services.storage().getFiles(project).then((files) => dispatch({type: appStateActions.switchQuestion, payload: {question: {name: name, files: files.filter((file) => file.name.split("/")[0] === name).map((file) => file.name)}}}));
              }
          },
          user: {
            signin: (username: string, password: string) => {
                if (trim(username) === "" || trim(password) === ""){
                    showError("Please fill in all fields!");
                } else {
                    asyncAction(Services.login(username, password)).then(function(response){
                        console.log(response);
                    }).catch(console.error);
                }
            }
          },
          project: {
              addProject: (newProjectName: string) => Services.storage().newProject(newProjectName).then(() => dispatch({type: appStateActions.addProject, payload: {name: newProjectName}})),
              removeProject: (name: string) => Services.storage().deleteProject(name).then(() => dispatch({type: appStateActions.removeProject, payload: {name: name}})),
              switchProject: (name: string) => {
                  // TODO interface with backend and get real data
                  // we will leave switching question and file to the UI
                  // efficiency is for noobs
                  function unique(val: any, idx: Number, arr: any){
                    return arr.indexOf(val) === idx;
                  }
                  Services.storage().getFiles(name).then((files) => dispatch({type: appStateActions.switchProject, payload: {project: {name: name, questions: files.map((file) => file.name).filter(unique)}}}));
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
