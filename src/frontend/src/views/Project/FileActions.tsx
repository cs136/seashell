import * as React from "react";
import {Menu, MenuItem, MenuDivider} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import * as S from "../../helpers/Storage/Interface";
const styles = require("./project.scss");

export interface FileActionsProps {
  file: S.FileBrief;
}

export interface FileActionsState {  }

class FileActions extends React.Component<FileActionsProps & actionsInterface, FileActionsState> {
  openFiles: any;
  constructor(props: FileActionsProps & actionsInterface) {
    super(props);
    if(!this.props.appState.currentProject||!this.props.appState.currentProject.currentQuestion){
      throw new Error("Invoking FileActions on undefined currentProject or currentQuestion!");
    }
    else{
      this.openFiles=this.props.appState.currentProject.currentQuestion.openFiles;
    }
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
        this.props.dispatch.dialog.toggleRenameFile();
      }}/>
      <MenuItem text="Copy" onClick={() => {
        this.props.dispatch.file.setFileOpTarget(this.props.file);
        this.props.dispatch.dialog.toggleCopyFile();
      }}/>
      <MenuItem text="Delete" onClick={() => {
        this.props.dispatch.file.setFileOpTarget(this.props.file);
        this.props.dispatch.dialog.toggleDeleteFile();
      }}/>
      <MenuDivider />
      <MenuItem text="Close File" onClick={() => {
        this.props.dispatch.file.closeFile(file, this.openFiles);
        let state = this.props.appState;

        if (state.currentProject && state.currentProject && state.currentProject.currentQuestion) {
          if (state.currentProject.currentQuestion.openFiles.length > 0) {
            this.props.dispatch.file.switchFile(state.currentProject.currentQuestion.openFiles[0]);
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

export default map<FileActionsProps>(FileActions);
