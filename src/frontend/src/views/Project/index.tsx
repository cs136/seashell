import * as React from "react";
import {Popover, Position, Tooltip} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import Question from "./Question";
import QuestionList from "./QuestionList";
import FileList from "./FileList";

const styles = require<any>("./project.scss");
const layoutStyles = require<any>("../../Layout.scss");


export interface ProjectProps { title: string; routeParams: any; }
export interface ProjectState { }



class Project extends React.Component<ProjectProps&actionsInterface, ProjectState> {
  constructor(props: ProjectProps&actionsInterface) {
    super(props);
  }
  render() {
    const project = this.props.appState.currentProject;
    const question = project.currentQuestion;
    return (<div><nav className={"pt-navbar " + styles.actionbar}>
        <div className="pt-navbar-group pt-align-left">
          <div className="pt-navbar-heading">{project.name}</div>
          <Popover content={<QuestionList project={project} />} position={Position.BOTTOM}>
              <button className="pt-button pt-intent-primary"><span className="pt-icon-standard pt-icon-caret-down" />{project.currentQuestion.name}</button>
          </Popover>
          <span className="pt-navbar-divider"></span>
          <Popover content={<FileList question={question}/>} position={Position.BOTTOM}>
              <button className="pt-button"><span className="pt-icon-standard pt-icon-caret-down" />Open File</button>
          </Popover>
        </div>
        <div className="pt-navbar-group pt-align-right">
          <Tooltip content="Test" position={Position.BOTTOM_LEFT}><button className="pt-button pt-minimal pt-icon-build"></button></Tooltip>
          <Tooltip content="Run" position={Position.BOTTOM_LEFT}><button className="pt-button pt-minimal pt-icon-play"></button></Tooltip>
          <Tooltip content="Submit to Marmoset" position={Position.BOTTOM_RIGHT}><button className="pt-button pt-minimal pt-icon-send-to"></button></Tooltip>
        </div>
      </nav>
      <Question question={question.name} />
      </div>);
  }
}

export default map<ProjectProps>(Project);
