import * as React from "react";
import {map, actionsInterface} from "../../../actions";
import {showError} from "../../../partials/Errors";
import Prompt from "./Prompt";
import Confirm from "./Confirm";

interface DeleteProps {
  closefunc: Function;
};

class Delete extends React.Component<DeleteProps&actionsInterface, {}> {

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
    return (<Confirm submitText="Delete" bodyText="Are you sure you want to delete this file?"
        submitfunc={() => this.submitForm()} closefunc={this.props.closefunc} />);
  }
}

export default map<DeleteProps>(Delete);
