import * as React from "react";
import {Menu, MenuItem, MenuDivider} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";

const styles = require<any>("./project.scss");

export interface FileActionsProps { file: string; };
export interface FileActionsState {  }

class FileActions extends React.Component<FileActionsProps & actionsInterface, FileActionsState> {
    constructor(props: FileActionsProps & actionsInterface) {
        super(props);
    }
    render() {
        const file = this.props.file;
        return (<Menu>
            <MenuItem text="Move/Rename" />
            <MenuItem text="Copy" />
            <MenuItem text="Delete" />
            <MenuDivider />
            <MenuItem text="Set as Run File" onClick={this.props.dispatch.file.setRunFile.bind(null, file)}/>
            <MenuDivider />
            <MenuItem text="Close File" onClick={this.props.dispatch.file.closeFile.bind(null, file)}/>
        </Menu>);
    }
}
export default map<FileActionsProps>(FileActions);