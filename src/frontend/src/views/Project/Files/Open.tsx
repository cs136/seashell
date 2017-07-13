import * as React from "react";
import {Menu, MenuItem, Popover, Position} from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";
import FileActions from "./Actions";

const styles = require("../Project.scss");

export interface OpenProps {}

export interface OpenState { };

class Open extends React.Component<OpenProps & actionsInterface, OpenState> {
  constructor(props: OpenProps & actionsInterface) {
    super(props);
  }

  render() {
    const project = this.props.appState.currentProject;
    if (project) {
      const question = project.currentQuestion;
      const switchFile = this.props.dispatch.file.switchFile;
      if (question) {
        return (<div className={styles.openFiles}>
          {question.openFiles.map((filename) => {
            const index = filename.indexOf(question.name);
            return (<div className={styles.openFilesTab} key={"file-tab-" + filename}>
              <div className={`pt-button-group ${styles.openFilesTab} ${(question.currentFile && filename === question.currentFile.name) ? styles.active : ""}`}>
                <button className={"pt-button pt-minimal " + styles.openFilesTabFile} onClick={switchFile.bind(null, project.id, filename)}>
                  {filename === question.runFile ? <span className="pt-icon-standard pt-icon-play" /> : null}
                  {filename.substring(index === -1 ? 0 : index + question.name.length + 1)}
                </button>
                <Popover content={<FileActions filename={filename}/>}
                        position={Position.BOTTOM}>
                  <button className={"pt-button pt-minimal pt-icon-caret-down " + styles.openFilesTabAction}>
                  </button>
                </Popover>
              </div></div>
            );
          })}
        </div>);
      } else {
        return <div></div>;
      }
    } else {
      return <div></div>;
    }
  }
}

export default map<OpenProps>(Open);
