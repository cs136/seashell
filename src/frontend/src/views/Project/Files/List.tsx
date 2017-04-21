import * as React from "react";
import {Menu, MenuItem} from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";

const styles = require("../Project.scss");

export interface ListProps { question: any; };
export interface ListState {  }

class List extends React.Component<ListProps & actionsInterface, ListState> {
    constructor(props: ListProps & actionsInterface) {
        super(props);
    }
    render() {
        const question = this.props.question;
        return (<Menu>
            <MenuItem iconName="plus" text="New File" />
            {question.files.map((file: any) => (<MenuItem key={"file-list-item-" + file} onClick={() => {
                this.props.dispatch.file.openFile(file);
                this.props.dispatch.file.switchFile(this.props.appState.currentProject.name, file);
            }} iconName="document" text={file.substring(file.indexOf("/") + 1)} />))}
        </Menu>);
    }
}
export default map<ListProps>(List);
