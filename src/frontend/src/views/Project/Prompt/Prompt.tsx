import * as React from "react";
import {merge} from "ramda";
import {map, actionsInterface} from "../../../actions";

export interface PromptProps {
  closefunc: Function;
  submitfunc: Function;
  submitMessage: string;
}

export interface PromptState {
  fieldsDisabled: boolean;
}

class Prompt extends React.Component<PromptProps&actionsInterface, PromptState> {
  constructor(props: PromptProps&actionsInterface) {
    super(props);
    this.state = {
      fieldsDisabled: false
    };
  }

  private submitForm() {
    this.setState(merge(this.state, {fieldsDisabled: true}));
    this.props.submitfunc().then(() =>
      this.setState(merge(this.state, {fieldsDisabled: false})));
  }

  render() {
    return(<div className="pt-dialog-body" onKeyPress={(e: any) => {
        if (e.key === "Enter") {
          this.submitForm();
        }}}>
      {React.Children.map(this.props.children, c =>
        React.isValidElement(c) ?
          React.cloneElement(c as React.ReactElement<any>, {disabled: this.state.fieldsDisabled}) :
          c)}
      <div className="pt-button-group">
        <button type="button" className="pt-button" onClick={() => this.props.closefunc()}>
          Cancel
        </button>
        <button type="button" className="pt-button pt-intent-primary"
            onClick={() => this.submitForm()}>
          {this.props.submitMessage}
        </button>
      </div>
    </div>);
  }
}

export default map<PromptProps>(Prompt);
