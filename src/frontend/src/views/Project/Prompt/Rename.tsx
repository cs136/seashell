import * as React from "react";
import {merge} from "ramda";
import {FileBrief} from "../../../helpers/Storage/Interface";
import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";
import {showError} from "../../../partials/Errors";
import Prompt from "./Prompt";

interface RenameProps {
  questions: string[];
  closefunc: Function;
}

interface RenameState {
  question: string;
  file: string;
  prevFile: string;
  target: string;
  disabled: boolean;
}

class Rename extends React.Component<RenameProps&actionsInterface, RenameState> {
  private openFiles: any;

  constructor(props: RenameProps&actionsInterface) {
    super(props);
    if (this.props.appState.fileOpTarget
        && this.props.appState.currentProject
        && this.props.appState.currentProject.currentQuestion) {
      this.state = {
        question: this.props.questions[0],
        file: this.props.appState.fileOpTarget.split("/").pop() || "",
        prevFile: this.props.appState.fileOpTarget.split("/").pop() || "",
        target: this.props.appState.fileOpTarget,
        disabled: false
      };
      this.openFiles = this.props.appState.currentProject.currentQuestion.openFiles;
    } else {
      throw new Error("Invoking RenameWindow on undefined fileOpTarget or currentProject or currentQuestion!");
    }
  }

  private submitForm(): Promise<void> {
    const project = this.props.appState.currentProject;
    if (project) {
      return this.props.dispatch.file.renameFile(project.id, this.state.target,
          `${this.state.question}/${this.state.file}`)
        .then((target) =>
          this.props.dispatch.question.switchQuestion(project.id, this.state.question)
          .then(() => {
            this.props.dispatch.file.openFile(project.id, this.state.question, target.name);
            this.props.dispatch.file.switchFile(project.id, target.name);
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
      <div>
        <div className="pt-select pt-fill">
          <select value={this.state.question} disabled={this.state.disabled} onChange={e =>
              this.setState(merge(this.state, {question: e.currentTarget.value}))}>
            {this.props.questions.map((question: string) =>
              (<option value={question}>{question}</option>))}
          </select>
        </div>
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
