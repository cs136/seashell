import * as React from "react";
import {Menu, MenuItem} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import File from "./File";

const styles = require("./project.scss");

export interface FileListProps { question: any; };
export interface FileListState {  }

class FileList extends React.Component<FileListProps & actionsInterface, FileListState> {
    constructor(props: FileListProps & actionsInterface) {
        super(props);
    }
    render() {
        const question = this.props.question;
        return (<Menu>
            <MenuItem iconName="plus" text="New File" />
            {question.files.map((file: any) => (<MenuItem key={"file-list-item-" + file} onClick={()=>{
                this.props.dispatch.file.openFile(file);
                this.props.dispatch.file.switchFile(this.props.appState.currentProject.name, file);
            }} iconName="document" text={file.substring(file.indexOf("/") + 1)} />))}
        </Menu>);
    }
}
export default map<FileListProps>(FileList);