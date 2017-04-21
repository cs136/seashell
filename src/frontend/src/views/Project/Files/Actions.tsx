import * as React from "react";
import {Menu, MenuItem, MenuDivider} from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";

const styles = require("../Project.scss");

export interface ActionsProps {
  file: string;
  toggleDelete: Function;
  toggleCopy: Function;
  toggleRename: Function;
}

export interface ActionsState {  }

class Actions extends React.Component<ActionsProps & actionsInterface, ActionsState> {
  constructor(props: ActionsProps & actionsInterface) {
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

export default map<ActionsProps>(Actions);
