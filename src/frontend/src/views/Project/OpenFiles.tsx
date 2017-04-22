import * as React from "react";
import {Menu, MenuItem, Popover, Position} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import FileActions from "./FileActions";

const styles = require("./project.scss");

export interface OpenFilesProps {}

export interface OpenFilesState { };

class OpenFiles extends React.Component<OpenFilesProps & actionsInterface, OpenFilesState> {
  constructor(props: OpenFilesProps & actionsInterface) {
    super(props);
  }

  render() {
    const project = this.props.appState.currentProject;
    if (project) {
      const question = project.currentQuestion;
      const switchFile = this.props.dispatch.file.switchFile;
      if (question) {
        return (<div className={styles.openFiles}>
          {question.openFiles.map((file) => (
            <div className={styles.openFilesTab} key={`file-tab-${file.id}`}>
              <div className={`pt-button-group ${styles.openFilesTab} ${(question.currentFile && file.id === question.currentFile.id) ? styles.active : ""}`}>
                <button className={"pt-button pt-minimal " + styles.openFilesTabFile} onClick={switchFile.bind(null, file)}>
                  {file.id === question.runFile ? <span className="pt-icon-standard pt-icon-play" /> : null}
                  {file.name.substring(file.name.indexOf("/") + 1)}
                </button>
                <Popover content={<FileActions file={file}/>}
                        position={Position.BOTTOM}>
                  <button className={"pt-button pt-minimal pt-icon-caret-down " + styles.openFilesTabAction}>
                  </button>
                </Popover>
              </div>
            </div>
          ))}
        </div>);
      } else {
        return <div></div>;
      }
    } else {
      return <div></div>;
    }
  }
}

export default map<OpenFilesProps>(OpenFiles);
