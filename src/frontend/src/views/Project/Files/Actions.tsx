import * as React from "react";
import {Menu, MenuItem, MenuDivider} from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";
import * as S from "../../../helpers/Storage/Interface";
const styles = require("../Project.scss");

export interface ActionsProps {
  filename: string;
}

export interface ActionsState {  }

class Actions extends React.Component<ActionsProps & actionsInterface, ActionsState> {
  openFiles: any;
  question: string;
  project: S.ProjectID;

  constructor(props: ActionsProps & actionsInterface) {
    super(props);
    if (!this.props.appState.currentProject || !this.props.appState.currentProject.currentQuestion) {
      throw new Error("Invoking FileActions on undefined currentProject or currentQuestion!");
    }
    else {
      this.openFiles = this.props.appState.currentProject.currentQuestion.openFiles;
      this.question = this.props.appState.currentProject.currentQuestion.name;
      this.project = this.props.appState.currentProject.id
    }
  }
  render() {
    const filename = this.props.filename;
    return (<Menu>
      <MenuItem text="Set as Run File" onClick={
        this.props.dispatch.file.setRunFile.bind(null, this.question, filename)
      }/>
      <MenuDivider />
      <MenuItem text="Rename/Move" onClick={() => {
        this.props.dispatch.file.setFileOpTarget(filename);
        this.props.dispatch.dialog.toggleRenameFile();
      }}/>
      <MenuItem text="Copy" onClick={() => {
        this.props.dispatch.file.setFileOpTarget(filename);
        this.props.dispatch.dialog.toggleCopyFile();
      }}/>
      <MenuItem text="Delete" onClick={() => {
        this.props.dispatch.file.setFileOpTarget(filename);
        this.props.dispatch.dialog.toggleDeleteFile();
      }}/>
      <MenuDivider />
      <MenuItem text="Close File" onClick={() => {
        this.props.dispatch.file.closeFile(this.project, this.question, filename);
        let state = this.props.appState;

        if (state.currentProject && state.currentProject && state.currentProject.currentQuestion) {
          if (state.currentProject.currentQuestion.openFiles.length > 0) {
            this.props.dispatch.file.switchFile(state.currentProject.id,
              state.currentProject.currentQuestion.openFiles[0]);
          }
          else {
            this.props.dispatch.file.invalidateFile();
          }
        } else {
          throw new Error("Tried to close file in an undefined project/question?!");
        }
      }}/>
    </Menu>);
  }
}

export default map<ActionsProps>(Actions);
