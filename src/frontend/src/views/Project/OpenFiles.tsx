import * as React from "react";
import {Menu, MenuItem, Popover, Position} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import FileActions from "./FileActions";

const styles = require("./project.scss");

export interface OpenFilesProps {toggleDelete: Function; toggleCopy: Function; toggleRename: Function };
export interface OpenFilesState { };

class OpenFiles extends React.Component<OpenFilesProps & actionsInterface, OpenFilesState> {
    constructor(props: OpenFilesProps & actionsInterface) {
        super(props);
    }
    render() {
        const question = this.props.appState.currentProject.currentQuestion;
        const switchFile = this.props.dispatch.file.switchFile;
        return (<div className={styles.openFiles}>
            {question.openFiles.map((file: string) => (
                <div className={styles.openFilesTab} key={"file-tab-" + file}>
                    <div className={"pt-button-group " + styles.openFilesTab + (file === question.currentFile.name ? " " + styles.active : "")}>
                        <button className={"pt-button pt-minimal " + styles.openFilesTabFile} onClick={switchFile.bind(null, this.props.appState.currentProject.name, file)}>{file === question.runFile ? <span className="pt-icon-standard pt-icon-play" /> : null}{file}</button>
                        <Popover content={<FileActions file={file} toggleCopy={this.props.toggleCopy} toggleDelete={this.props.toggleDelete} toggleRename={this.props.toggleRename}/>} position={Position.BOTTOM}>
                            <button className={"pt-button pt-minimal pt-icon-caret-down " + styles.openFilesTabAction}></button>
                        </Popover>
                    </div>
                </div>
            ))}
        </div>);
    }
}
export default map<OpenFilesProps>(OpenFiles);