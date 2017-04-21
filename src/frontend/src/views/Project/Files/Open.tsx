import * as React from "react";
import {Menu, MenuItem, Popover, Position} from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";
import FileActions from "./Actions";

const styles = require("../Project.scss");

export interface OpenProps {
  toggleDelete: Function;
  toggleCopy: Function;
  toggleRename: Function;
}

export interface OpenState { };

class Open extends React.Component<OpenProps & actionsInterface, OpenState> {
  constructor(props: OpenProps & actionsInterface) {
    super(props);
  }

  render() {
    const question = this.props.appState.currentProject.currentQuestion;
    const switchFile = this.props.dispatch.file.switchFile;
    return (<div className={styles.openFiles}>
      {question.openFiles.map((file: string) => (
        <div className={styles.openFilesTab} key={"file-tab-" + file}>
          <div className={"pt-button-group " + styles.openFilesTab
              + (file === question.currentFile.name ? " " + styles.active : "")}>
            <button className={"pt-button pt-minimal " + styles.openFilesTabFile}
                onClick={switchFile.bind(null, this.props.appState.currentProject.name, file)}>
              {file === question.runFile ?
                <span className="pt-icon-standard pt-icon-play" /> :
                null}
              {file.substring(file.indexOf("/") + 1)}
            </button>
            <Popover content={<FileActions file={file} toggleCopy={this.props.toggleCopy}
                  toggleDelete={this.props.toggleDelete} toggleRename={this.props.toggleRename}/>}
                position={Position.BOTTOM}>
              <button className=
                {"pt-button pt-minimal pt-icon-caret-down " + styles.openFilesTabAction}>
              </button>
            </Popover>
          </div>
        </div>
      ))}
    </div>);
  }
}

export default map<OpenProps>(Open);
