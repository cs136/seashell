import * as React from "react";
import {Popover, Position, Tooltip} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import DisplayFiles from "./Files/Display";
import QuestionList from "./QuestionList";
import ListFiles from "./Files/List";
import OpenFiles from "./Files/Open";
import Navigation from "../../partials/Navigation";
import {Dialog} from "@blueprintjs/core";
const styles = require("./Project.scss");
const layoutStyles = require("../../Layout.scss");
import {merge} from "ramda";
import CopyPrompt from "./Prompt/Copy";
import RenamePrompt from "./Prompt/Rename";
import DeletePrompt from "./Prompt/Delete";
import Splash from "./Splash";
import {RouteComponentProps} from "react-router";
import { showError } from "../../partials/Errors";
export interface ProjectProps extends RouteComponentProps<{}> { title: string; }
export interface ProjectState { deleteVisible: boolean; renameVisible: boolean; copyVisible: boolean; toggleView: boolean; }



class Project extends React.Component<ProjectProps&actionsInterface, ProjectState> {
  constructor(props: ProjectProps&actionsInterface) {
    super(props);
    this.state = {
        toggleView: false,
        deleteVisible: false,
        renameVisible: false,
        copyVisible: false,
    };
  }
  componentWillMount() {
    if (this.props.location.pathname.split("/").pop() !== this.props.appState.currentProject.name) {
        this.props.dispatch.file.invalidateFile();
        // force wait until promise is resolved
        this.props.dispatch.project.switchProject(this.props.location.pathname.split("/").pop()).then(() => {
            if (this.props.appState.currentProject.questions.length > 0) {
                /*this.props.dispatch.question.switchQuestion(this.props.appState.currentProject.name, this.props.appState.currentProject.questions[0]).catch(
                    (reason) => {
                      if (reason !== null) {
                        showError(reason.message);
                      }
                    }
                );*/
            }
        }).catch((reason) => {
          if (reason !== null) {
            showError(reason.message);
          }
        });
    }
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

    const navLeft = [
        <div className="pt-navbar-heading" key="project-name">{project.name}</div>,

        <Popover content={<QuestionList project={project} />} key="project-question" position={Position.BOTTOM}>
            <button className="pt-button pt-intent-primary"><span className="pt-icon-standard pt-icon-caret-down" />{project.currentQuestion.name || "Select a Question"}</button>
        </Popover>,

        <span className="pt-navbar-divider" key="project-divider"></span>,

        (question.name !== "" ?
            <Popover content={<ListFiles question={question}/>} position={Position.BOTTOM} key="project-open-file">
                <button className="pt-button"><span className="pt-icon-standard pt-icon-caret-down" />Open File</button>
            </Popover> :
            null)];

    const navRight = [
            <OpenFiles key="project-open-files" />,
            <Tooltip key="project-toggle-view" content="Toggle Editor/Console" position={Position.BOTTOM}><button onClick={this.toggleView.bind(this)} className={"pt-button pt-minimal pt-icon-applications " + styles.toggleView}></button></Tooltip>,

            <Tooltip key="project-build-file" content="Test" position={Position.BOTTOM_RIGHT}>
                {this.props.appState.runState !== 0 || question.runFile === "" ? 
                    <button className="pt-button pt-minimal pt-disabled pt-icon-comparison"></button> :
                    <button className="pt-button pt-minimal pt-icon-comparison" onClick={() => this.props.dispatch.file.flushFileBuffer().then(this.props.dispatch.compile.compileAndRun.bind(this, this.props.appState.currentProject.name, question.name, question.runFile, true))}></button>}
            </Tooltip>,

            (question.runFile === "" ?
                (<Tooltip key="project-run-file-set" content="Please set a run file" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-disabled pt-icon-play"></button></Tooltip>) :
                (this.props.appState.runState === 0 ?
                    (<Tooltip key="project-run-file" content="Run" position={Position.BOTTOM_RIGHT}>
                        <button className="pt-button pt-minimal pt-icon-play" onClick={() => this.props.dispatch.file.flushFileBuffer().then(this.props.dispatch.compile.compileAndRun.bind(this, this.props.appState.currentProject.name, question.name, question.runFile, false))}></button></Tooltip>) :
                    (this.props.appState.runState === 1 ?
                        <Tooltip key="project-run-file" content="Compiling" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-disabled pt-icon-build"></button></Tooltip> :
                        <Tooltip key="project-run-file" content="Stop" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-icon-stop" onClick={() => this.props.dispatch.compile.stopProgram()}></button></Tooltip>))),

            <Tooltip key="project-submit-marmoset" content="Submit to Marmoset" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-icon-publish-function"></button></Tooltip>];

    return (<div>

        <Navigation navLeft={navLeft} navRight={navRight} />

        {this.props.appState.currentProject.currentQuestion.currentFile.name === "" ?
            <Splash project={project}/> :
            <DisplayFiles className={this.state.toggleView ? styles.rightToggle : styles.leftToggle} file={question.currentFile.name} />}

        <Dialog className={styles.dialogStyle} title="Delete File" isOpen={this.state.deleteVisible} onClose={this.toggleDelete.bind(this)}><DeletePrompt closefunc={this.toggleDelete.bind(this)}/></Dialog>

        <Dialog className={styles.dialogStyle} title="Rename/Move File" isOpen={this.state.renameVisible} onClose={this.toggleRename.bind(this)}><RenamePrompt questions={this.props.appState.currentProject.questions} closefunc={this.toggleRename.bind(this)}/></Dialog>

        <Dialog className={styles.dialogStyle} title="Copy File" isOpen={this.state.copyVisible} onClose={this.toggleCopy.bind(this)}><CopyPrompt questions={this.props.appState.currentProject.questions} closefunc={this.toggleCopy.bind(this)}/></Dialog>

        </div>);
  }
}

export default map<ProjectProps>(Project);
