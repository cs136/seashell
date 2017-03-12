import * as React from "react";
import {Popover, Position, Tooltip} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import File from "./File";
import QuestionList from "./QuestionList";
import FileList from "./FileList";
import OpenFiles from "./OpenFiles";
import Navigation from "../../partials/Navigation";
import {Dialog} from "@blueprintjs/core";
const styles = require<any>("./project.scss");
const layoutStyles = require<any>("../../Layout.scss");
import {merge} from "ramda";
import CopyWindow from "./CopyWindow";
import RenameWindow from "./RenameWindow";
import DeleteWindow from "./DeleteWindow";


export interface ProjectProps { title: string; routeParams: any; }
export interface ProjectState { deleteVisible: boolean; renameVisible: boolean; copyVisible: boolean, fileOpTarget: string}



class Project extends React.Component<ProjectProps&actionsInterface, ProjectState> {
  constructor(props: ProjectProps&actionsInterface) {
    super(props);
    this.state={
        deleteVisible: false,
        renameVisible: false,
        copyVisible: false,
        fileOpTarget: this.props.appState.currentProject.currentQuestion.currentFile.name
    };
  }
  changeTargetFile(file: string){
      this.setState(merge(this.state, {fileOpTarget: file}));
  }
  toggleDelete(){
      this.setState(merge(this.state, {deleteVisible: !this.state.deleteVisible}));
  }
  toggleRename(){
      this.setState(merge(this.state, {renameVisible: !this.state.renameVisible}));
  }
  toggleCopy(){
      this.setState(merge(this.state, {copyVisible: !this.state.copyVisible}));
  }
  render() {
    const project = this.props.appState.currentProject;
    const question = project.currentQuestion;
    return (<div><Navigation navLeft={[
          <div className="pt-navbar-heading">{project.name}</div>,
          <Popover content={<QuestionList project={project} />} position={Position.BOTTOM}>
              <button className="pt-button pt-intent-primary"><span className="pt-icon-standard pt-icon-caret-down" />{project.currentQuestion.name}</button>
          </Popover>,
          <span className="pt-navbar-divider"></span>,
          <Popover content={<FileList question={question}/>} position={Position.BOTTOM}>
              <button className="pt-button"><span className="pt-icon-standard pt-icon-caret-down" />Open File</button>
          </Popover>,
          <OpenFiles setTargetFile={this.changeTargetFile.bind(this)} toggleDelete={this.toggleDelete.bind(this)} toggleCopy={this.toggleCopy.bind(this)} toggleRename={this.toggleRename.bind(this)}/>]} navRight={[
          <Tooltip content="Test" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-icon-build"></button></Tooltip>,
          question.runFile === null ? <Tooltip content="Please set a run file" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-disabled pt-icon-play"></button></Tooltip> : <Tooltip content="Run" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-icon-play"></button></Tooltip>,
          <Tooltip content="Submit to Marmoset" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-icon-send-to"></button></Tooltip>]} />
      <File file={question.currentFile.name} />
      <Dialog className={styles.dialogStyle} title="Delete File" isOpen={this.state.deleteVisible} onClose={this.toggleDelete.bind(this)}><DeleteWindow file={this.state.fileOpTarget} closefunc={this.toggleDelete.bind(this)}/></Dialog>
      <Dialog className={styles.dialogStyle} title="Rename/Move File" isOpen={this.state.renameVisible} onClose={this.toggleRename.bind(this)}><RenameWindow questions={this.props.appState.currentProject.questions} file={this.state.fileOpTarget} closefunc={this.toggleRename.bind(this)}/></Dialog>
      <Dialog className={styles.dialogStyle} title="Copy File" isOpen={this.state.copyVisible} onClose={this.toggleCopy.bind(this)}><CopyWindow questions={this.props.appState.currentProject.questions} file={this.state.fileOpTarget} closefunc={this.toggleCopy.bind(this)}/></Dialog>
      </div>);
  }
}

export default map<ProjectProps>(Project);
