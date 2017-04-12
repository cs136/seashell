import * as React from "react";

import {merge} from "ramda";

import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";

import {showError} from "../../partials/Errors";

export interface DeleteWindowProps {
  closefunc: Function;
};

class DeleteWindow extends React.Component<DeleteWindowProps&actionsInterface, {}> {
  render() {
    if (this.props.appState.fileOpTarget &&
        this.props.appState.currentProject &&
        this.props.appState.currentProject.currentQuestion) {
      let target = this.props.appState.fileOpTarget;
      let project = this.props.appState.currentProject;
      let question = this.props.appState.currentProject.currentQuestion;
      return (<div className="pt-dialog-body">
        <p>Are you sure you want to delete this file?</p>
        <div className="pt-button-group">
          <button type="button" className="pt-button" onClick={() => {
                  this.props.closefunc();
                  }}>Cancel</button>
          <button type="button" className="pt-button pt-intent-danger" onClick={() => {
            this.props.dispatch.file.deleteFile(target).then(
              () => {
                if (question.openFiles.length > 0) {
                  this.props.dispatch.file.switchFile(question.openFiles[0]);
                }
              }
            ).catch((error: any) => {
              if (error !== null) {
                showError(error.message);
              }
            });
            this.props.closefunc();
            }}>Delete</button>
        </div>
      </div>
      );
    } else {
      throw new Error("DeleteFile invoked in invalid state!");
    }
  }
}

export default map<DeleteWindowProps>(DeleteWindow);
