import * as React from "react";

import {merge} from "ramda";
import {FileBrief} from "../../helpers/Storage/Interface";
import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";

import {showError} from "../../partials/Errors";

export interface RenameWindowProps {
  questions: string[];
  closefunc: Function;
}

class RenameWindow extends React.Component<RenameWindowProps&actionsInterface,
  {question: string; file: string, prevFile: string, target: FileBrief}> {
  openFiles: any;
  constructor(props: RenameWindowProps&actionsInterface) {
    super(props);
    console.log(this.props.appState.fileOpTarget);
    if (this.props.appState.fileOpTarget&&this.props.appState.currentProject&&this.props.appState.currentProject.currentQuestion) {
      this.state = {
        question: this.props.questions[0],
        file: this.props.appState.fileOpTarget.name.split("/").pop() || "",
        prevFile: this.props.appState.fileOpTarget.name.split("/").pop() || "",
        target: this.props.appState.fileOpTarget
      };
      this.openFiles=this.props.appState.currentProject.currentQuestion.openFiles;
    } else {
      throw new Error("Invoking RenameWindow on undefined fileOpTarget or currentProject or currentQuestion!");
    }
  }
  render() {
    const project = this.props.appState.currentProject;
    if (project) {
      return (<div className="pt-dialog-body">
        <p>Where would you like to rename/move this file?</p>
        <div><div className="pt-select pt-fill"><select id="question" value={this.state.question} onChange={(e) => this.setState(merge(this.state, {question: e.currentTarget.value}))}>
          {this.props.questions.map((question: string) => (<option value={question}>{question}</option>))}
          </select></div>
          <input className="pt-input pt-fill" required type="text" value={this.state.file}
          onBlur={() => {
            if (this.state.file === "" || this.state.file.includes("/")) {
              this.setState(merge(this.state, {file: this.state.prevFile}));
            }
            else {
              this.setState(merge(this.state, {prevFile: this.state.file}));
            }
          }}
          onChange={(e => this.setState(merge(this.state, {file: e.currentTarget.value})))}/></div>
        <div className="pt-button-group">
          <button type="button" className="pt-button" onClick={() => {
                  this.props.closefunc();
                  }}>Cancel</button>
          <button type="button" className="pt-button pt-intent-primary" onClick={() => {
            this.props.dispatch.file.renameFile(this.state.target, this.state.question + "/" + this.state.file).then(
              () => this.props.dispatch.question.switchQuestion(project.id, this.state.question).then(() => {
                this.props.dispatch.file.openFile(this.state.target, this.openFiles);
                this.props.dispatch.file.switchFile(this.state.target);
                })
            ).catch((error) => {
              if (error !== null) {
                showError(error.message);
              }
            });
            this.props.closefunc();
            }}>Rename/Move</button>
        </div>
      </div>
      );
    } else {
      throw new Error("Invoking RenameWindow on undefined project!");
    }
  }
}

export default map<RenameWindowProps>(RenameWindow);
