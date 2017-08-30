import {actionsInterface, getDispatch} from "./actions";
import {Store} from "redux";
import {bind} from "mousetrap";
export default (store: Store<any>) => {

    const dispatch = getDispatch(store.dispatch).dispatch;

    const withState = (fn: Function) => ((e: Event) => {
        if (e.preventDefault) e.preventDefault(); else e.returnValue = false;
        fn({dispatch: dispatch, ...store.getState()});
    });
/*
    // Previous Tab
    bind("mod+shift+a", withState((state: actionsInterface) => {
        const question = state.appState.currentProject.currentQuestion;
        if (question.currentFile === null) return;
        const pos = question.openFiles.indexOf(question.currentFile.name);
        if (pos === -1) return;
        let newPos = pos === 0 ? question.openFiles.length - 1 : pos - 1;
        state.dispatch.file.switchFile(question.openFiles[newPos]);
    }));

    // Next Tab
    bind("mod+shift+s", withState((state: actionsInterface) => {
        const question = state.appState.currentProject.currentQuestion;
        if (question.currentFile === null) return;
        const pos = question.openFiles.indexOf(question.currentFile.name);
        if (pos === -1) return;
        let newPos = (pos + 1) % question.openFiles.length;
        state.dispatch.file.switchFile(question.openFiles[newPos]);
    }));*/

};