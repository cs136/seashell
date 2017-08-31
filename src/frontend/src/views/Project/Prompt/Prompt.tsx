import * as React from "react";
import {merge} from "ramda";
import {map, actionsInterface} from "../../../actions";

interface PromptProps {
  closefunc: Function;
  submitfunc: () => Promise<any>;
  disable?: (val: boolean) => void;
  submitMessage: string;
}

interface PromptState {
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
    if (this.props.disable) this.props.disable(true);
    this.props.submitfunc().then(() => {
      if (this.props.disable) this.props.disable(false);
      this.props.closefunc();
    });
  }

  render() {
    return(<div className="pt-dialog-body" onKeyPress={(e: any) => {
        if (e.key === "Enter") {
          this.submitForm();
          e.stopPropagation();
          e.preventDefault();
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
            onClick={(e: any) => {
              this.submitForm();
              e.stopPropagation();
              e.preventDefault();
            }} autoFocus>
          {this.props.submitMessage}
        </button>
      </div>
    </div>);
  }
}

export default map<PromptProps>(Prompt);
