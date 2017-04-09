import * as React from "react";
import {Menu, MenuItem, MenuDivider} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";

const styles = require("./project.scss");

export interface FileActionsProps {
  file: string;
  toggleDelete: Function;
  toggleCopy: Function;
  toggleRename: Function;
}

export interface FileActionsState {  }

class FileActions extends React.Component<FileActionsProps & actionsInterface, FileActionsState> {
  constructor(props: FileActionsProps & actionsInterface) {
    super(props);
  }
  render() {
    const file = this.props.file;
    return (<Menu>
      <MenuItem text="Set as Run File" onClick={
        this.props.dispatch.file.setRunFile.bind(null, file)
      }/>
      <MenuDivider />
      <MenuItem text="Rename/Move" onClick={() => {
        this.props.dispatch.file.setFileOpTarget(this.props.file);
        this.props.toggleRename();
      }}/>
      <MenuItem text="Copy" onClick={() => {
        this.props.dispatch.file.setFileOpTarget(this.props.file);
        this.props.toggleCopy();
      }}/>
      <MenuItem text="Delete" onClick={() => {
        this.props.dispatch.file.setFileOpTarget(this.props.file);
        this.props.toggleDelete();
      }}/>
      <MenuDivider />
      <MenuItem text="Close File" onClick={() => {
        this.props.dispatch.file.closeFile.bind(null, file);
        if (this.props.appState.currentProject.currentQuestion.openFiles.length > 0) {
          this.props.dispatch.file.switchFile(
            this.props.appState.currentProject.name,
            this.props.appState.currentProject.currentQuestion.openFiles[0]);
        }
        else {
          this.props.dispatch.file.invalidateFile();
        }
      }}/>
    </Menu>);
  }
}

export default map<FileActionsProps>(FileActions);
