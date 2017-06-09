import * as React from "react";
import {map, actionsInterface} from "../../../actions";
import Prompt from "./Prompt";

interface ConfirmProps {
  bodyText: string;
  submitText: string;
  submitfunc: () => Promise<any>;
  closefunc: Function;
};

class Confirm extends React.Component<ConfirmProps&actionsInterface, {}> {

  render() {
    return (<Prompt submitMessage={this.props.submitText} submitfunc={this.props.submitfunc}
        closefunc={this.props.closefunc}>
      <p>{this.props.bodyText}</p>
    </Prompt>
    );
  }
}

export default map<ConfirmProps>(Confirm);
