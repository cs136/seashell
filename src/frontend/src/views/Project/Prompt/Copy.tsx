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
  question: string;
  file: string;
  prevFile: string;
  disabled: boolean;
};

class Copy extends React.Component<CopyProps&actionsInterface, CopyState> {

  constructor(props: CopyProps&actionsInterface) {
    super(props);
    let filename = this.props.appState.fileOpTarget;
    if (filename) {
      this.state = {
        question: this.props.questions[0],
        file: filename.split("/").pop() || "", // Both of these are unreachable
        prevFile: filename.split("/").pop() || "", // As above ^ ^
        disabled: false
      };
    } else {
      throw new Error("CopyWindow invoked on undefined file!");
    }
  }

  private submitForm(): Promise<void> {
    return this.props.dispatch.file.copyFile(`${this.state.question}/${this.state.file}`);
  }

  render() {
    return(<Prompt submitMessage="Copy" submitfunc={() => this.submitForm()}
        closefunc={this.props.closefunc}
        disable={(val: boolean) => this.setState(merge(this.state, {disabled: val}))}>
      <p>Where would you like to copy this file?</p>
      <div>
        <div className="pt-select pt-fill">
          <select id="question" value={this.state.question}
            onChange={(e) => this.setState(merge(this.state, {question: e.currentTarget.value}))}>
            {this.props.questions.map((question: string) =>
              (<option value={question}>{question}</option>))}
          </select>
        </div>
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
