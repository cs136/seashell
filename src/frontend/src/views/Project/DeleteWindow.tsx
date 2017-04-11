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
    return (<div className="pt-dialog-body">
      <p>Are you sure you want to delete this file?</p>
      <div className="pt-button-group">
        <button type="button" className="pt-button" onClick={() => {
                this.props.closefunc();
                }}>Cancel</button>
        <button type="button" className="pt-button pt-intent-danger" onClick={() => {
          this.props.dispatch.file.deleteFile(this.props.appState.fileOpTarget).then(
            () => {
              if (this.props.appState.currentProject.currentQuestion.openFiles.length > 0) {
                this.props.dispatch.file.switchFile(this.props.appState.currentProject.currentQuestion.openFiles[0]);
              }
            }
          ).catch((error) => {
            if (error !== null) {
              showError(error.message);
            }
          });
          this.props.closefunc();
          }}>Delete</button>
      </div>
    </div>
    );
  }
}

export default map<DeleteWindowProps>(DeleteWindow);
