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
import AddFileWindow from "./NewFile";
import Splash from "./Splash";
import Loading from "./Loading";
import {RouteComponentProps} from "react-router";
import { showError } from "../../partials/Errors";
import * as S from "../../helpers/Storage/Interface";
export interface ProjectProps extends RouteComponentProps<{}> { title: string; }
export interface ProjectState { toggleView: boolean; }


class Project extends React.Component<ProjectProps&actionsInterface, ProjectState> {
  constructor(props: ProjectProps&actionsInterface) {
    super(props);
    this.state = {
        toggleView: false
    };
  }
  componentWillMount() {
    const willOpenPid: S.ProjectID = this.props.location.pathname.split("/").pop() || "";
    let state = this.props.appState;
    if (!state.currentProject ||
        willOpenPid !== state.currentProject.id) {
        if (state.currentProject) this.props.dispatch.file.invalidateFile();
        // force wait until promise is resolved
        this.props.dispatch.project.switchProject(willOpenPid).then(() => {
            if (this.props.appState.currentProject&&this.props.appState.currentProject.questions.length > 0) {
                this.props.dispatch.question.switchQuestion(this.props.appState.currentProject.id, this.props.appState.currentProject.questions[0]).catch(
                    (reason) => {
                      if (reason !== null) {
                        showError(reason.message);
                      }
                    }
                );
            }
        }).catch((reason) => {
          if (reason !== null) {
            showError(reason.message);
          }
        });
    }
  }
  toggleView() {
      this.setState(merge(this.state, {toggleView: !this.state.toggleView}));
  }
  render() {
    const project = this.props.appState.currentProject;
    if (project) {
        const question = project.currentQuestion;
        if (question) {
            return (<div>
                <Navigation navLeft={[
                        <div className="pt-navbar-heading" key="project-name">{project.name}</div>,
                        <Popover content={<QuestionList />} key="project-question" position={Position.BOTTOM}>
                            <button className="pt-button pt-intent-primary"><span className="pt-icon-standard pt-icon-caret-down" />{question.name || "Select a Question"}</button>
                        </Popover>,
                        <span className="pt-navbar-divider" key="project-divider" />,
                        <Popover content={<FileList question={question}/>} position={Position.BOTTOM} key="project-open-file">
                            <button className="pt-button"><span className="pt-icon-standard pt-icon-caret-down" />Open File</button>
                        </Popover>]}
                    navRight={[
                        <OpenFiles key="project-open-files" />,
                        <Tooltip key="project-toggle-view" content="Toggle Editor/Console" position={Position.BOTTOM}>
                            <button onClick={this.toggleView.bind(this)} className={"pt-button pt-minimal pt-icon-applications " + styles.toggleView}>
                            </button>
                        </Tooltip>,
                        <Tooltip key="project-build-file" content="Test" position={Position.BOTTOM_RIGHT}>
                            {this.props.appState.runState !== 0 || question.runFile === "" ?
                                <button className="pt-button pt-minimal pt-disabled pt-icon-comparison"> </button>
                                :
                                <button className="pt-button pt-minimal pt-icon-comparison"
                                    onClick={() =>
                                        this.props.dispatch.file.flushFileBuffer().then(this.props.dispatch.compile.compileAndRun.bind(this, project.name, question.name, question.runFile, true))}>
                                </button>}
                        </Tooltip>,
                        question.runFile ?
                            <Tooltip key="project-run-file-set" content="Please set a run file" position={Position.BOTTOM_RIGHT}>
                            <button className="pt-button pt-minimal pt-disabled pt-icon-play"></button></Tooltip>
                        : this.props.appState.runState === 0 ?
                            <Tooltip key="project-run-file" content="Run" position={Position.BOTTOM_RIGHT}>
                            <button className="pt-button pt-minimal pt-icon-play"
                                onClick={() => this.props.dispatch.file.flushFileBuffer().then(this.props.dispatch.compile.compileAndRun.bind(this, project.name, question.name, question.runFile, false))}>
                            </button>
                            </Tooltip>
                            : this.props.appState.runState === 1 ?
                                <Tooltip key="project-run-file" content="Compiling" position={Position.BOTTOM_RIGHT}>
                                    <button className="pt-button pt-minimal pt-disabled pt-icon-build">
                                </button>
                                </Tooltip>
                                : <Tooltip key="project-run-file" content="Stop" position={Position.BOTTOM_RIGHT}>
                                    <button className="pt-button pt-minimal pt-icon-stop" onClick={() => this.props.dispatch.compile.stopProgram()}>
                                    </button>
                                 </Tooltip>,
                        <Tooltip key="project-submit-marmoset" content="Submit to Marmoset" position={Position.BOTTOM_RIGHT}>
                            <button className="pt-button pt-minimal pt-icon-publish-function">
                            </button>
                        </Tooltip>]}/>
                {!question.currentFile ?
                    <Splash/> :
                    <File className={this.state.toggleView ? styles.rightToggle : styles.leftToggle}
                        file={question.currentFile.name} />}
                <Dialog className={styles.dialogStyle} title="Delete File"
                    isOpen={this.props.dialog.delete_file_open}
                    onClose={this.props.dispatch.dialog.toggleDeleteFile}>
                        <DeleteWindow closefunc={this.props.dispatch.dialog.toggleDeleteFile}/>
                </Dialog>
                <Dialog className={styles.dialogStyle} title="Rename/Move File"
                    isOpen={this.props.dialog.rename_file_open}
                    onClose={this.props.dispatch.dialog.toggleRenameFile}>
                    <RenameWindow questions={project.questions} closefunc={this.props.dispatch.dialog.toggleRenameFile}/>
                </Dialog>
                <Dialog className={styles.dialogStyle} title="Copy File"
                    isOpen={this.props.dialog.copy_file_open}
                    onClose={this.props.dispatch.dialog.toggleCopyFile}>
                        <CopyWindow questions={project.questions} closefunc={this.props.dispatch.dialog.toggleCopyFile}/>
                </Dialog>
                <Dialog className={styles.dialogStyle} title="Add File"
                    isOpen={this.props.dialog.add_file_open}
                    onClose={this.props.dispatch.dialog.toggleAddFile}>
                        <AddFileWindow questions={project.questions} closefunc={this.props.dispatch.dialog.toggleAddFile}/>
                </Dialog>
            </div>);
        } else {
            return <Splash/>;
        }
    } else {
        return <Loading/>;
    }
  }
}

export default map<ProjectProps>(Project);
