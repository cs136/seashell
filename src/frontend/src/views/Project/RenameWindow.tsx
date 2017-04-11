import * as React from "react";

import {merge} from "ramda";

import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";

import {showError} from "../../partials/Errors";

export interface RenameWindowProps {
  questions: string[];
  closefunc: Function;
}

class RenameWindow extends React.Component<RenameWindowProps&actionsInterface, {question: string; file: string, prevFile: string}> {
  constructor(props: RenameWindowProps&actionsInterface) {
    super(props);
    console.log(this.props.appState.fileOpTarget);
    this.state = {
      question: this.props.questions[0],
      file: this.props.appState.fileOpTarget.name.split("/").pop(),
      prevFile: this.props.appState.fileOpTarget.name.split("/").pop()
    };
  }
  render() {
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
          this.props.dispatch.file.renameFile(this.props.appState.fileOpTarget, this.state.question + "/" + this.state.file).then(
            () => this.props.dispatch.question.switchQuestion(this.props.appState.currentProject.id, this.state.question).then(() => {
              this.props.dispatch.file.openFile(this.props.appState.fileOpTarget);
              this.props.dispatch.file.switchFile(this.props.appState.fileOpTarget);
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
  }
}

export default map<RenameWindowProps>(RenameWindow);
