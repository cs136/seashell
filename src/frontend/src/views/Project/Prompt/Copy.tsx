import * as React from "react";

import {merge} from "ramda";

import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";

export interface CopyProps {questions: string[]; closefunc: Function; };

class Copy extends React.Component<CopyProps&actionsInterface, {question: string; file: string, prevFile: string, fieldsDisabled: boolean}> {

  constructor(props: CopyProps&actionsInterface) {
    super(props);
    let file = this.props.appState.fileOpTarget;
    if (file) {
      this.state = {
        question: this.props.questions[0],
        file: file.name.split("/").pop() || "", // Both of these are unreachable
        prevFile: file.name.split("/").pop() || "", // As above ^ ^
        fieldsDisabled: false
      };
    } else {
      throw new Error("CopyWindow invoked on undefined file!");
    }
  }

  private submitForm() {
    this.props.dispatch.file.copyFile(this.state.question + "/" + this.state.file);
    this.props.closefunc();
  }

  render() {
    return(<div className="pt-dialog-body">
      <p>Where would you like to copy this file?</p>
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
        <button type="button" className="pt-button pt-intent-primary"
          onClick={() => this.submitForm()}>Copy</button>
      </div>
    </div>
    );
  }
}

export default map<CopyProps>(Copy);
