import * as React from "react";
import {merge} from "ramda";
import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";
import {showError} from "../../../partials/Errors";
import Prompt from "./Prompt";

interface DeleteProps {
  closefunc: Function;
};

interface DeleteState {
  disabled: boolean;
}

class Delete extends React.Component<DeleteProps&actionsInterface, DeleteState> {

  constructor(props: DeleteProps&actionsInterface) {
    super(props);
    this.state = {
      disabled: false
    };
  }

  private submitForm(): Promise<void> {
    if (this.props.appState.fileOpTarget &&
        this.props.appState.currentProject &&
        this.props.appState.currentProject.currentQuestion) {
      const target = this.props.appState.fileOpTarget;
      const project = this.props.appState.currentProject;
      const question = this.props.appState.currentProject.currentQuestion;
      return this.props.dispatch.file.deleteFile(target).then(() => {
        if (question.openFiles.length > 0) {
          this.props.dispatch.file.switchFile(question.openFiles[0]);
        }
      }).catch((error: any) => {
        if (error !== null) {
          showError(error.message);
        }
      });
    }
    return Promise.reject("DeleteFile called in an invalid state.");
  }

  render() {
    return (<Prompt submitMessage="Delete" submitfunc={() => this.submitForm()}
        closefunc={this.props.closefunc} disable={(val: boolean) =>
          this.setState(merge(this.state, {disabled: val}))}>
      <p>Are you sure you want to delete this file?</p>
    </Prompt>
    );
  }
}

export default map<DeleteProps>(Delete);
