import * as React from "react";

import {merge} from "ramda";

import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";

export interface AddFileProps {questions: string[]; closefunc: Function; };

class AddFile extends React.Component<AddFileProps&actionsInterface, { file: string, prevFile: string}> {
  project: string;
  question: string;
  constructor(props: AddFileProps&actionsInterface) {
    super(props);
    if (this.props.appState.currentProject && this.props.appState.currentProject.currentQuestion) {
      this.project = this.props.appState.currentProject.id;
      this.question = this.props.appState.currentProject.currentQuestion.name;
      this.state = {
        file: "",
        prevFile: ""
      };
    } else {
      throw new Error("AddFile invoke on undefined project!");
    }
  }
  render() {
    return(<div className="pt-dialog-body">
      <p>What would you like to call this file?</p>
      <div>
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
        <button type="button" className="pt-button pt-intent-primary" disabled = {this.state.file === "" || this.state.file.includes("/")} onClick={() => {
          this.props.dispatch.file.addFile(this.project, this.question + "/" + this.state.file,
          this.state.file.split(".").pop() === "c" ? "int main(){\n\treturn 0;\n}" :
          this.state.file.split(".").pop() === "h" ? "//put your interface here\n" :
          this.state.file.split(".").pop() === "rkt" ? "#lang racket\n" : "").then(() => this.props.closefunc());
          }}>Add File</button>
      </div>
    </div>
    );
  }
}

export default map<AddFileProps>(AddFile);
