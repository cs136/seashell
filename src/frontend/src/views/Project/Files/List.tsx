import * as React from "react";
import {Menu, MenuItem} from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";
import * as S from "../../../helpers/Storage/Interface";

const styles = require("../Project.scss");

class List extends React.Component<actionsInterface, {}> {
  project: S.ProjectID;
  question: any;

  constructor(props: actionsInterface) {
    super(props);
    if (!this.props.appState.currentProject || !this.props.appState.currentProject.currentQuestion) {
      throw new Error("FileList initiated on undefined project or question");
    }
    else {
      this.project = this.props.appState.currentProject.id;
      this.question = this.props.appState.currentProject.currentQuestion;
    }
  }

  render() {
    if (this.props.appState.currentProject) {
        this.project = this.props.appState.currentProject.id;
        this.question = this.props.appState.currentProject.currentQuestion;
    }
    return (<Menu>
        <MenuItem iconName="plus" text="New File" onClick={() => this.props.dispatch.dialog.toggleAddFile()}/>
        <MenuItem iconName="plus" text="New Test" onClick={() => this.props.dispatch.dialog.toggleAddTest()}/>
          {this.question.files.map((filename: string) => (<MenuItem key={"file-list-item-" + filename} onClick={() => {
            this.props.dispatch.file.openFile(this.project, this.question.name, filename);
            this.props.dispatch.file.switchFile(this.project, this.question.name, filename);
          }} iconName="document" text={
            filename.startsWith("common") ? filename : filename.substring(filename.indexOf("/") + 1)} />))}
    </Menu>);
  }
}
export default map<{}>(List);
