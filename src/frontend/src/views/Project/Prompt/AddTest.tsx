import * as React from "react";
import {merge} from "ramda";
import {map, actionsInterface} from "../../../actions";
import {showError} from "../../../partials/Errors";
import Prompt from "./Prompt";

interface AddTestProps {
  questions: string[];
  closefunc: Function;
}

interface AddTestState {
  test: string;
  prevTest: string;
  disabled: boolean;
}

class AddTest extends React.Component<AddTestProps&actionsInterface, AddTestState> {
  project: string;
  question: string;

  constructor(props: AddTestProps&actionsInterface) {
    super(props);
    if (this.props.appState.currentProject && this.props.appState.currentProject.currentQuestion) {
      this.project = this.props.appState.currentProject.id;
      this.question = this.props.appState.currentProject.currentQuestion.name;
      this.state = {
        test: "",
        prevTest: "",
        disabled: false
      };
    } else {
      throw new Error("AddTest invoke on undefined project!");
    }
  }

  private submitForm() {
    return Promise.all([
      this.props.dispatch.file.addFile(
        this.project,
        `${this.project}/${this.state.test}.in`,
        ""),
      this.props.dispatch.file.addFile(
        this.project,
        `${this.project}/${this.state.test}.expect`,
        "")
    ]);
  }

  render() {
    return(<Prompt submitMessage="Add Test" closefunc={this.props.closefunc}
        submitfunc={() => this.submitForm()} disable={(val: boolean) =>
          this.setState(merge(this.state, {disabled: val}))}>
      <p>What would you like to call this test?</p>
      <div>
        <label>New Test:
          <input className="pt-input pt-fill" required disabled={this.state.disabled}
            type="text" value={this.state.test} ref={input => input && input.focus()}
          onBlur={() => {
            if (this.state.test === "" || this.state.test.includes("/")) {
              this.setState(merge(this.state, {test: this.state.prevTest}));
            }
            else {
              this.setState(merge(this.state, {prevTest: this.state.test}));
            }
          }}
          onChange={(e => this.setState(merge(this.state, {test: e.currentTarget.value})))}/>
        </label>
      </div>
    </Prompt>
    );
  }
}

export default map<AddTestProps>(AddTest);
