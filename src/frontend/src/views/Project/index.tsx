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
import MarmosetResultPrompt from "./Prompt/MarmosetResult";
import AddFilePrompt from "./Prompt/AddFile";
import AddTestPrompt from "./Prompt/AddTest";
import Splash from "./Splash";
import Loading from "./Loading";
import {RouteComponentProps} from "react-router";
import { showError } from "../../partials/Errors";
import {appStateReducerProjectState} from "../../reducers/appStateReducer";
import * as S from "../../helpers/Storage/Interface";
import ProjectMenu from "./ProjectMenu";

export interface ProjectProps extends RouteComponentProps
    <{name: string, id: S.ProjectID}> { title: string; }
export interface ProjectState { toggleView: boolean; marmosetResults: {open: boolean; result: any}; }


class Project extends React.Component<ProjectProps&actionsInterface, ProjectState> {
  constructor(props: ProjectProps&actionsInterface) {
    super(props);
    this.state = {
      toggleView: false,
      marmosetResults: {
        open: false,
        result: null
      }
    };
  }
  generateMarmosetButton(project: appStateReducerProjectState) {
      const projectPattern = /A[0-9]+/, questionPattern = /[qp][0-9]+/;
      if (project.currentQuestion && project.name.match(projectPattern) && project.currentQuestion.name.match(questionPattern)) {
        const marmosetDispatch = (async (projectName: string, questionName: string) => {
          this.setState({marmosetResults: {
            open: true,
            result: null
          }});
          const result = await this.props.dispatch.question.getMarmosetResults(projectName, questionName);
          this.setState({marmosetResults: {
            open: true,
            result: result
          }});
        }).bind(this, project.name, project.currentQuestion.name);
        return (<Tooltip key="project-submit-marmoset" content="Submit to Marmoset" position={Position.BOTTOM_RIGHT}>
                <button className="pt-button pt-minimal pt-icon-publish-function" onClick={marmosetDispatch}>
                </button>
            </Tooltip>);
      }
      return <span />;
  }
  componentWillMount() {
    const willOpenPid = this.props.match.params.id;
    const willOpenName = this.props.match.params.name;
    let state = this.props.appState;
    if (!state.currentProject || willOpenPid !== state.currentProject.id) {
      if (state.currentProject) {
        this.props.dispatch.file.invalidateFile();
      }
      // force wait until promise is resolved
      this.props.dispatch.project.switchProject(willOpenName, willOpenPid).catch((reason) => {
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
      return (<div>
        <Navigation navLeft={[
            <Popover content={<ProjectMenu />} position={Position.BOTTOM_LEFT}>
              <div className="pt-navbar-heading pt-button" key="project-name">{project.name}</div>
            </Popover>,
            <Popover content={<QuestionList />} key="project-question" position={Position.BOTTOM}>
              <button className="pt-button pt-intent-primary">
                <span className="pt-icon-standard pt-icon-caret-down" />
                {question ? question.name : "Select a Question"}
              </button>
            </Popover>,
            question ?
              <span className="pt-navbar-divider" key="project-divider" /> :
              <span key="empty-project-divider" />,
            question ?
              <Popover content={<ListFiles question={question}/>}
                  position={Position.BOTTOM} key="project-open-file">
                <button className="pt-button">
                  <span className="pt-icon-standard pt-icon-caret-down" />
                  Open File
                </button>
              </Popover> :
              <span key="empty-project-open-file"/>
          ]}
          navRight={question ? [
            <OpenFiles key="project-open-files" />,
            <Tooltip key="project-toggle-view" content="Toggle Editor/Console"
                position={Position.BOTTOM}>
              <button onClick={this.toggleView.bind(this)}
                  className={"pt-button pt-minimal pt-icon-applications " + styles.toggleView}>
              </button>
            </Tooltip>,
            <Tooltip key="project-build-file" content="Test" position={Position.BOTTOM_RIGHT}>
              {this.props.appState.runState !== 0 || !question.runFile ?
                <button className="pt-button pt-minimal pt-disabled pt-icon-comparison"></button> :
                <button className="pt-button pt-minimal pt-icon-comparison"
                  onClick={() =>
                    this.props.dispatch.file.flushFileBuffer()
                      .then(this.props.dispatch.compile.compileAndRun
                        .bind(this, project.id, question.name, question.runFile, true))}>
                </button>
              }
            </Tooltip>,
            !question.runFile ?
              <Tooltip key="project-run-file-set" content="Please set a run file"
                  position={Position.BOTTOM_RIGHT}>
                <button className="pt-button pt-minimal pt-disabled pt-icon-play"></button>
              </Tooltip> :
              this.props.appState.runState === 0 ?
                <Tooltip key="project-run-file" content="Run" position={Position.BOTTOM_RIGHT}>
                  <button className="pt-button pt-minimal pt-icon-play"
                    onClick={() =>
                    this.props.dispatch.compile.compileAndRun(project.id, question.name, question.runFile, false)}>
                  </button>
                </Tooltip> :
                this.props.appState.runState === 1 ?
                  <Tooltip key="project-run-file" content="Compiling"
                      position={Position.BOTTOM_RIGHT}>
                    <button className="pt-button pt-minimal pt-disabled pt-icon-build">
                    </button>
                  </Tooltip> :
                  <Tooltip key="project-run-file" content="Stop" position={Position.BOTTOM_RIGHT}>
                    <button className="pt-button pt-minimal pt-icon-stop"
                        onClick={() => this.props.dispatch.compile.stopProgram()}>
                    </button>
                  </Tooltip>,
            this.generateMarmosetButton(project)] : []
          }/>
        {question && question.currentFile ?
          <DisplayFiles className={this.state.toggleView ? styles.rightToggle : styles.leftToggle}/>
            : <Splash />}
        <Dialog className={styles.dialogStyle} title="Delete File"
            isOpen={this.props.dialog.delete_file_open}
            onClose={this.props.dispatch.dialog.toggleDeleteFile}>
          <DeletePrompt closefunc={this.props.dispatch.dialog.toggleDeleteFile}/>
        </Dialog>
        <Dialog className={styles.dialogStyle} title="Rename/Move File"
            isOpen={this.props.dialog.rename_file_open}
            onClose={this.props.dispatch.dialog.toggleRenameFile}>
          <RenamePrompt questions={project.questions}
              closefunc={this.props.dispatch.dialog.toggleRenameFile}/>
        </Dialog>
        <Dialog className={styles.dialogStyle} title="Copy File"
            isOpen={this.props.dialog.copy_file_open}
            onClose={this.props.dispatch.dialog.toggleCopyFile}>
          <CopyPrompt questions={project.questions}
              closefunc={this.props.dispatch.dialog.toggleCopyFile}/>
        </Dialog>
        <Dialog className={styles.dialogStyle} title="Add File"
            isOpen={this.props.dialog.add_file_open}
            onClose={this.props.dispatch.dialog.toggleAddFile}>
          <AddFilePrompt questions={project.questions}
              closefunc={this.props.dispatch.dialog.toggleAddFile}/>
        </Dialog>
        <Dialog className={styles.dialogStyle} title="Add Test"
            isOpen={this.props.dialog.add_test_open}
            onClose={this.props.dispatch.dialog.toggleAddTest}>
          <AddTestPrompt questions={project.questions}
              closefunc={this.props.dispatch.dialog.toggleAddTest}/>
        </Dialog>
        <Dialog className={styles.dialogStyle} title="Marmoset Results"
            isOpen={this.state.marmosetResults.open}
            onClose={(() => {
              this.setState({marmosetResults: {open: false, result: this.state.marmosetResults.result}});
            }).bind(this)}>
                <MarmosetResultPrompt result={this.state.marmosetResults.result} />
        </Dialog>
      </div>);
    } else {
      return <Loading/>;
    }
  }
}

export default map<ProjectProps>(Project);
