import * as React from "react";
import {merge} from "ramda";
import {map, actionsInterface} from "../../../actions";
import {showError} from "../../../partials/Errors";
import {ProjectID} from "../../../helpers/Services";
import Prompt from "./Prompt";

interface AddQuestionProps {
  closefunc: Function;
}

interface AddQuestionState {
  question: string;
  prevQuestion: string;
  disabled: boolean;
}

class AddQuestion extends React.Component<AddQuestionProps&actionsInterface, AddQuestionState> {
  private project: ProjectID;

  constructor(props: AddQuestionProps&actionsInterface) {
    super(props);
    if (this.props.appState.currentProject) {
      this.state = {
        question: "",
        prevQuestion: "",
        disabled: false
      };
    } else {
      throw new Error("AddTest invoked on undefined project!");
    }
  }

  private submitForm() {
    return this.props.dispatch.question.addQuestion(this.project, this.state.question);
  }

  render() {
    return(<Prompt submitMessage="Add Question" closefunc={this.props.closefunc}
        submitfunc={() => this.submitForm()} disable={(val: boolean) =>
          this.setState(merge(this.state, {disabled: val}))}>
      <p>What would you like to call this question?</p>
      <div>
        <label>New Question:
          <input className="pt-input pt-fill" required disabled={this.state.disabled}
            type="text" value={this.state.question} ref={input => input && input.focus()}
          onBlur={() => {
            if (this.state.question === "" || this.state.question.includes("/")) {
              this.setState(merge(this.state, {question: this.state.prevQuestion}));
            }
            else {
              this.setState(merge(this.state, {prevQuestion: this.state.question}));
            }
          }}
          onChange={(e => this.setState(merge(this.state, {question: e.currentTarget.value})))}/>
        </label>
      </div>
    </Prompt>
    );
  }
}

export default map<AddQuestionProps>(AddQuestion);
