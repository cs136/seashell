import * as React from "react";
import {Menu, MenuItem} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import File from "./File";
import * as S from "../../helpers/Storage/Interface";

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
            <MenuItem iconName="plus" text="New File" onClick={() => this.props.dispatch.dialog.toggleAddFile()}/>
            {question.files.map((file: S.FileBrief) => (<MenuItem key={"file-list-item-" + file.name} onClick={() => {
                this.props.dispatch.file.openFile(file);
                this.props.dispatch.file.switchFile(file);
            }} iconName="document" text={file.name.substring(file.name.indexOf("/") + 1)} />))}
        </Menu>);
    }
}
export default map<FileListProps>(FileList);
