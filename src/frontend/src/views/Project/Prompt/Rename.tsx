import * as React from "react";
import {merge} from "ramda";
import {File} from "../../../helpers/Storage/Interface";
import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";
import {showError} from "../../../partials/Errors";
import Prompt from "./Prompt";

interface RenameProps {
  questions: string[];
  closefunc: Function;
}

interface RenameState {
  commonOrQuestion: string;
  file: string;
  prevFile: string;
  target: string;
  disabled: boolean;
  sourceQuestion: string;
}

class Rename extends React.Component<RenameProps&actionsInterface, RenameState> {
  private openFiles: any;

  constructor(props: RenameProps&actionsInterface) {
    super(props);
    if (this.props.appState.fileOpTarget
        && this.props.appState.currentProject
        && this.props.appState.currentProject.currentQuestion) {
      // File name contains <common|question>/<rest-of-file-name>
      let source = this.props.appState.fileOpTarget.split("/")[0];
      this.state = {
        commonOrQuestion: source,
        file: this.props.appState.fileOpTarget.split("/").pop() || "",
        prevFile: this.props.appState.fileOpTarget.split("/").pop() || "",
        target: this.props.appState.fileOpTarget,
        disabled: false,
        sourceQuestion: this.props.appState.currentProject.currentQuestion.name
      };
      this.openFiles = this.props.appState.currentProject.currentQuestion.openFiles;
    } else {
      throw new Error("Invoking RenameWindow on undefined fileOpTarget or currentProject or currentQuestion!");
    }
  }

  private submitForm(): Promise<void> {
    const project = this.props.appState.currentProject;
    let question = this.state.commonOrQuestion === "common" ? this.state.sourceQuestion : this.state.commonOrQuestion;
    if (project) {
      return this.props.dispatch.file.renameFile(project.id, question, this.state.target,
          `${this.state.commonOrQuestion}/${this.state.file}`)
        .then((target) =>
          this.props.dispatch.question.switchQuestion(project.id, question)
          .then(() => {
            this.props.dispatch.file.openFile(project.id, question, target.name);
            this.props.dispatch.file.switchFile(project.id, question, target.name);
            })
        ).catch((error: any) => {
          if (error !== null) {
            showError(error.message);
          }
        });
    }
    return Promise.reject("Calling RenameFile in invalid state.");
  }

  render() {
    return (<Prompt submitMessage="Rename/Move" submitfunc={() => this.submitForm()}
        closefunc={this.props.closefunc} disable={(val: boolean) =>
          this.setState(merge(this.state, {disabled: val}))}>
      <p>Where would you like to rename/move this file?</p>
      <div classname="pt-form-group">
        <div className="pt-select pt-fill">
          <select value={this.state.commonOrQuestion} disabled={this.state.disabled} onChange={e =>
              this.setState(merge(this.state, {commonOrQuestion: e.currentTarget.value}))}>
            <option value="common">common</option>
            {this.props.questions.map((question: string) =>
              (<option key={question} value={question}>{question}</option>))}
          </select>
        </div>
      </div>
      <div className="pt-form-group">
        <input className="pt-input pt-fill" required type="text" value={this.state.file}
          disabled={this.state.disabled}
          onBlur={() => {
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

export default map<RenameProps>(Rename);
