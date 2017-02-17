import { connect } from 'react-redux';
import { ComponentClass } from 'react';
import { globalState } from '../reducers/';
import {projectRef, fileRef} from '../types';
import {appStateActions} from '../reducers/appStateReducer';
import {settingsActions, settingsReducerState} from '../reducers/settingsReducer';
import {fileActions, fileReducerState} from '../reducers/fileReducer';

interface Func<T> {
    ([...args]: any): T;
}

function returnType<T>(func: Func<T>) {
    return null as T;
}

const mapStoreToProps = (state:globalState) => state;

const mapDispatchToProps = (dispatch:Function) => {
    return {
        dispatch: {
          settings: {
             updateSettings:(newSettings: settingsReducerState)=>dispatch({type: settingsActions.updateSettings, payload: newSettings})
          },
          file: {
              updateFile:(newFileContent: fileReducerState)=>dispatch({type: fileActions.changeContent, payload: newFileContent})
          }
        }
    }
};

const actionsStoreType = returnType(mapDispatchToProps);
export type actionsInterface = typeof actionsStoreType & globalState;

export function map<PropertyType>(Component:ComponentClass<any>){
    return connect<{},{},PropertyType>(mapStoreToProps,mapDispatchToProps)(Component);
}
