import { connect } from "react-redux";
import { ComponentClass } from "react";
import { globalState } from "../reducers/";
import {projectRef, fileRef} from "../types";
import {appStateActions} from "../reducers/appStateReducer";
import {settingsActions, settingsReducerState} from "../reducers/settingsReducer";

interface Func<T> {
    ([...args]: any): T;
}

function returnType<T>(func: Func<T>) {
    return null as T;
}

const mapStoreToProps = (state: globalState) => state;

const mapDispatchToProps = (dispatch: Function) => {
    return {
        dispatch: {
          settings: {
             updateSettings: (newSettings: settingsReducerState) => dispatch({type: settingsActions.updateSettings, payload: newSettings})
          },
          file: {
              updateFile: (newFileContent: string) => dispatch({type: appStateActions.changeFileContent, payload: newFileContent}),
              addFile: (newFileName: string, newFileContent: string) => dispatch({type: appStateActions.addFile, payload: {name: newFileName, content: newFileContent}}),
              deleteFile: (name: string) => dispatch({type: appStateActions.removeFile, payload: {name: name}}),
              switchFile: (name: string) => {
                  //TODO interface with backend and get real data
                  dispatch({type: appStateActions.switchFile, payload: {file: {name: name, content: "good"}}})}
          },
          question: {
              addQuestion: (newQuestionName: string) => dispatch({type: appStateActions.addQuestion, payload: {name: newQuestionName}}),
              removeQuestion: (name: string) => dispatch({type: appStateActions.removeQuestion, payload: {name: name}}),
              switchQuestion: (name: string) => {
                  //TODO interface with backend and get real data
                  //we will leave switch files to the UI
                  dispatch({type: appStateActions.switchQuestion, payload: {question: {name: name, files: ["file1.txt"]}}});
              }
          },
          project: {
              addProject: (newProjectName: string) => dispatch({type: appStateActions.addProject, payload: {name: newProjectName}}),
              removeProject: (name: string) => dispatch({type: appStateActions.removeProject, payload: {name: name}}),
              switchProject: (name: string) => {
                  //TODO interface with backend and get real data
                  //we will leave switch question and file to the UI
                  dispatch({type: appStateActions.switchProject, payload: {project: {name: name, questions: ["q1"]}}});
              }
          }
        }
    };
};

const actionsStoreType = returnType(mapDispatchToProps);
export type actionsInterface = typeof actionsStoreType & globalState;

export function map<PropertyType>(Component: ComponentClass<any>) {
    return connect<{}, {}, PropertyType>(mapStoreToProps, mapDispatchToProps)(Component);
}
