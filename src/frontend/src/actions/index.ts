import { connect } from "react-redux";
import { ComponentClass } from "react";
import { globalState } from "../reducers/";
import {projectRef, fileRef} from "../types";
import {appStateActions} from "../reducers/appStateReducer";
import { Services, GenericError, LoginError } from "../helpers/Services";
import { showError } from "../partials/Errors";
import {settingsActions, settingsReducerState} from "../reducers/settingsReducer";

interface Func<T> {
    ([...args]: any): T;
}

function returnType<T>(func: Func<T>) {
    return null as T;
}

async function asyncAction<T>(pr: Promise<T>) {
    try {
        return await pr;
    }catch (e) {
        switch (e.type) {
            case LoginError:
                showError("You need to sign in to perform this action");
            default:
                throw e;
        }
    }
}

const mapStoreToProps = (state: globalState) => state;

const mapDispatchToProps = (dispatch: Function) => {

    return {
        dispatch: {
          settings: {
             updateSettings: (newSettings: settingsReducerState) => dispatch({type: settingsActions.updateSettings, payload: newSettings}),
             updateEditorRatio: (ratio: number) => dispatch({type: settingsActions.updateEditorRatio, payload: ratio})
          },
          file: {
              copyFile: (targetName: string) => dispatch({type: appStateActions.copyFile, payload: {question: {name: "question", files: ["file1.txt"]}, newName: targetName.split("/").pop()}}),
              updateFile: (newFileContent: string) => {dispatch({type: appStateActions.changeFileContent, payload: newFileContent}); },
              addFile: (newFileName: string, newFileContent: string) => dispatch({type: appStateActions.addFile, payload: {name: newFileName, content: newFileContent}}),
              deleteFile: (name: string) => dispatch({type: appStateActions.removeFile, payload: {name: name}}),
              renameFile: (targetName: string) => dispatch({type: appStateActions.renameFile, payload: {question: {name: "question", files: ["file1.txt"]}, newName: targetName.split("/").pop()}}),
              openFile: (name: string) => dispatch({type: appStateActions.openFile, payload: name}),
              closeFile: (name: string) => dispatch({type: appStateActions.closeFile, payload: name}),
              setRunFile: (name: string) => dispatch({type: appStateActions.setRunFile, payload: name}),
              switchFile: (name: string) => {
                    // TODO interface with backend and get real data
                    dispatch({type: appStateActions.switchFile, payload: {file: {name: name, content: "good"}}});
                }
          },
          question: {
              addQuestion: (newQuestionName: string) => dispatch({type: appStateActions.addQuestion, payload: {name: newQuestionName}}),
              removeQuestion: (name: string) => dispatch({type: appStateActions.removeQuestion, payload: {name: name}}),
              switchQuestion: (name: string) => {
                  // TODO interface with backend and get real data
                  // we will leave switching files to the UI
                  dispatch({type: appStateActions.switchQuestion, payload: {question: {name: name, files: ["file1.txt"]}}});
              }
          },
          project: {
              addProject: (newProjectName: string) => dispatch({type: appStateActions.addProject, payload: {name: newProjectName}}),
              removeProject: (name: string) => dispatch({type: appStateActions.removeProject, payload: {name: name}}),
              switchProject: (name: string) => {
                  // TODO interface with backend and get real data
                  // we will leave switching question and file to the UI
                  dispatch({type: appStateActions.switchProject, payload: {project: {name: name, questions: ["q1"]}}});
              }
          },
          app: {
              // removeError: (errorIDX: Number) => dispatch({type: appStateActions.removeError, payload: {errorIDX: errorIDX}})

          }
        }
    };
};

const actionsStoreType = returnType(mapDispatchToProps);


export function getDispatch<actionsStoreType>(dispatch: Function){
    return mapDispatchToProps(dispatch);
}

export type actionsInterface = typeof actionsStoreType & globalState;

export function map<PropertyType>(Component: ComponentClass<any>) {
    return connect<{}, {}, PropertyType>(mapStoreToProps, mapDispatchToProps)(Component);
}
