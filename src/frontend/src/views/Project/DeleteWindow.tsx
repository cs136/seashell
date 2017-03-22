import * as React from "react";

import {merge} from "ramda";

import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";

export interface DeleteWindowProps {file: string; closefunc: Function};

class DeleteWindow extends React.Component<DeleteWindowProps&actionsInterface, {}>{
  render(){
    return(<div className="pt-dialog-body">
      <p>Are you sure you want to delete this file?</p>
      <div className="pt-button-group">
        <button type="button" className="pt-button" onClick={() => {
                this.props.closefunc();
                }}>Cancel</button>
        <button type="button" className="pt-button pt-intent-danger" onClick={() => {
          this.props.dispatch.file.deleteFile(this.props.appState.currentProject.name, this.props.appState.currentProject.currentQuestion.name + "/" + this.props.file);
          this.props.closefunc();
          }}>Delete</button>
      </div>
    </div>
    );
  }
}

export default map<DeleteWindowProps>(DeleteWindow);