import * as React from "react";
import {merge} from "ramda";
import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";
import Prompt from "./Prompt";

interface CopyProps {
  questions: string[];
  closefunc: Function;
};

interface CopyState {
  commonOrQuestion: string;
  file: string;
  prevFile: string;
  disabled: boolean;
  sourceQuestion: string;
};

class Copy extends React.Component<CopyProps&actionsInterface, CopyState> {

  constructor(props: CopyProps&actionsInterface) {
    super(props);
    let filename = this.props.appState.fileOpTarget;
    if (filename &&
        this.props.appState.currentProject &&
        this.props.appState.currentProject.currentQuestion) {
      let source = filename.split("/")[0];
      this.state = {
        commonOrQuestion: source,
        file: filename.split("/").pop() || "", // Both of these are unreachable
        prevFile: filename.split("/").pop() || "", // As above ^ ^
        disabled: false,
        sourceQuestion: this.props.appState.currentProject.currentQuestion.name
      };
    } else {
      throw new Error("CopyWindow invoked on undefined file or project or question!");
    }
  }

  private submitForm(): Promise<any> {
    let source = this.props.appState.fileOpTarget;
    let project = this.props.appState.currentProject;
    let question = this.state.commonOrQuestion === "common" ? this.state.sourceQuestion : this.state.commonOrQuestion;
    if (source && project)
      return this.props.dispatch.file.copyFile(project.id, question, source, `${this.state.commonOrQuestion}/${this.state.file}`);
    else
      return Promise.reject(new Error("Could not copy file -- invalid state reached.!"));
  }

  render() {
    return(<Prompt submitMessage="Copy" submitfunc={() => this.submitForm()}
        closefunc={this.props.closefunc}
        disable={(val: boolean) => this.setState(merge(this.state, {disabled: val}))}>
      <p>Where would you like to copy this file?</p>
      <div className="pt-form-group">
        <div className="pt-select pt-fill">
          <select id="question" value={this.state.commonOrQuestion}
            onChange={(e) => this.setState(merge(this.state, {commonOrQuestion: e.currentTarget.value}))}>
            {this.props.questions.map((question: string) =>
              (<option key={question} value={question}>{question}</option>))}
            <option value="common">common</option>
          </select>
        </div>
      </div>
      <div className="pt-form-group">
        <input className="pt-input pt-fill" required type="text" value={this.state.file}
          disabled={this.state.disabled} ref={input => input && input.focus()} onBlur={() => {
            if (this.state.file === "" || this.state.file.includes("/")) {
              this.setState(merge(this.state, {file: this.state.prevFile}));
            }
            else {
              this.setState(merge(this.state, {prevFile: this.state.file}));
            }
          }}
          onChange={(e => this.setState(merge(this.state, {file: e.currentTarget.value})))}/>
      </div>
    </Prompt>
    );
  }
}

export default map<CopyProps>(Copy);
