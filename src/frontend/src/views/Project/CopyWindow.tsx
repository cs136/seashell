import * as React from "react";

import {merge} from "ramda";

import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";

export interface CopyWindowProps {questions: string[]; closefunc: Function; };

class CopyWindow extends React.Component<CopyWindowProps&actionsInterface, {question: string; file: string, prevFile: string}>{
  constructor(props: CopyWindowProps&actionsInterface){
    super(props);
    this.state = {
      question: this.props.questions[0],
      file: this.props.appState.fileOpTarget.split("/").pop(),
      prevFile: this.props.appState.fileOpTarget.split("/").pop()
    };
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
        <button type="button" className="pt-button pt-intent-primary" onClick={() => {
          this.props.dispatch.file.copyFile(this.state.question + "/" + this.state.file);
          this.props.closefunc();
          }}>Copy</button>
      </div>
    </div>
    );
  }
}

export default map<CopyWindowProps>(CopyWindow);