import * as React from "react";
import {Popover, Position, Tooltip} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import File from "./File";
import QuestionList from "./QuestionList";
import FileList from "./FileList";
import OpenFiles from "./OpenFiles";
import Navigation from "../../partials/Navigation";
import {Dialog} from "@blueprintjs/core";
const styles = require("./project.scss");
const layoutStyles = require("../../Layout.scss");
import {merge} from "ramda";
import CopyWindow from "./CopyWindow";
import RenameWindow from "./RenameWindow";
import DeleteWindow from "./DeleteWindow";


export interface ProjectProps { title: string; routeParams: any; }
export interface ProjectState { deleteVisible: boolean; renameVisible: boolean; copyVisible: boolean; fileOpTarget: string; toggleView: boolean; }



class Project extends React.Component<ProjectProps&actionsInterface, ProjectState> {
  constructor(props: ProjectProps&actionsInterface) {
    super(props);
    this.state = {
        toggleView: false,
        deleteVisible: false,
        renameVisible: false,
        copyVisible: false,
        fileOpTarget: this.props.appState.currentProject.currentQuestion.currentFile.name
    };
  }
  changeTargetFile(file: string){
      this.setState(merge(this.state, {fileOpTarget: file}));
  }
  toggleDelete() {
      this.setState(merge(this.state, {deleteVisible: !this.state.deleteVisible}));
  }
  toggleRename() {
      this.setState(merge(this.state, {renameVisible: !this.state.renameVisible}));
  }
  toggleCopy() {
      this.setState(merge(this.state, {copyVisible: !this.state.copyVisible}));
  }
  toggleView() {
      this.setState(merge(this.state, {toggleView: !this.state.toggleView}));
  }
  render() {
    const project = this.props.appState.currentProject;
    const question = project.currentQuestion;
    return (<div><Navigation navLeft={[
          <div className="pt-navbar-heading" key="project-name">{project.name}</div>,
          <Popover content={<QuestionList project={project} />} key="project-question" position={Position.BOTTOM}>
              <button className="pt-button pt-intent-primary"><span className="pt-icon-standard pt-icon-caret-down" />{project.currentQuestion.name}</button>
          </Popover>,
          <span className="pt-navbar-divider" key="project-divider"></span>,
          <Popover content={<FileList question={question}/>} position={Position.BOTTOM} key="project-open-file">
              <button className="pt-button"><span className="pt-icon-standard pt-icon-caret-down" />Open File</button>
          </Popover>]} navRight={[
          <OpenFiles key="project-open-files" setTargetFile={this.changeTargetFile.bind(this)} toggleDelete={this.toggleDelete.bind(this)} toggleCopy={this.toggleCopy.bind(this)} toggleRename={this.toggleRename.bind(this)}/>,
          <Tooltip key="project-toggle-view" content="Toggle Editor/Console" position={Position.BOTTOM}><button onClick={this.toggleView.bind(this)} className={"pt-button pt-minimal pt-icon-applications " + styles.toggleView}></button></Tooltip>,
          <Tooltip key="project-build-file" content="Test" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-icon-comparison"></button></Tooltip>,
          question.runFile === null ? <Tooltip key="project-run-file-set" content="Please set a run file" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-disabled pt-icon-play"></button></Tooltip> : <Tooltip key="project-run-file" content="Run" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-icon-play"></button></Tooltip>,
          <Tooltip key="project-submit-marmoset" content="Submit to Marmoset" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-icon-publish-function"></button></Tooltip>]} />
      <File className={this.state.toggleView ? styles.rightToggle : styles.leftToggle} file={question.currentFile.name} />
      <Dialog className={styles.dialogStyle} title="Delete File" isOpen={this.state.deleteVisible} onClose={this.toggleDelete.bind(this)}><DeleteWindow file={this.state.fileOpTarget} closefunc={this.toggleDelete.bind(this)}/></Dialog>
      <Dialog className={styles.dialogStyle} title="Rename/Move File" isOpen={this.state.renameVisible} onClose={this.toggleRename.bind(this)}><RenameWindow questions={this.props.appState.currentProject.questions} file={this.state.fileOpTarget} closefunc={this.toggleRename.bind(this)}/></Dialog>
      <Dialog className={styles.dialogStyle} title="Copy File" isOpen={this.state.copyVisible} onClose={this.toggleCopy.bind(this)}><CopyWindow questions={this.props.appState.currentProject.questions} file={this.state.fileOpTarget} closefunc={this.toggleCopy.bind(this)}/></Dialog>
      </div>);
  }
}

export default map<ProjectProps>(Project);
