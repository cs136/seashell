import * as React from "react";
import {Menu, MenuItem} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import File from "./File";

const styles = require<any>("./project.scss");

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
            {question.files.map((file: any) => (<MenuItem key={"file-list-item-" + file} onClick={this.props.dispatch.file.openFile.bind(null, file)} iconName="document" text={question.name + "/" + file} />))}
        </Menu>);
    }
}
export default map<FileListProps>(FileList);