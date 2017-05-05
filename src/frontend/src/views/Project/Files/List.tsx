import * as React from "react";
import {Menu, MenuItem} from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";
import * as S from "../../../helpers/Storage/Interface";

const styles = require("../Project.scss");

export interface ListProps { question: any; };
export interface ListState {  }

class List extends React.Component<ListProps & actionsInterface, ListState> {
    openFiles: any;
    constructor(props: ListProps & actionsInterface) {
        super(props);
        if (!this.props.appState.currentProject || !this.props.appState.currentProject.currentQuestion) {
            throw new Error("FileList initiated on undefined project or question");
        }
        else {
            this.openFiles = this.props.appState.currentProject.currentQuestion;
        }
    }
    render() {
        const question = this.props.question;
        return (<Menu>
            <MenuItem iconName="plus" text="New File" onClick={() => this.props.dispatch.dialog.toggleAddFile()}/>
            {question.files.map((file: S.FileBrief) => (<MenuItem key={"file-list-item-" + file.name} onClick={() => {
                this.props.dispatch.file.openFile(file);
                this.props.dispatch.file.switchFile(file);
            }} iconName="document" text={
                file.question() === "common" ? file.name : file.name.substring(file.name.indexOf("/") + 1)} />))}
        </Menu>);
    }
}
export default map<ListProps>(List);
