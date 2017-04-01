import * as React from "react";
import {Link} from "react-router-dom";

import {map, actionsInterface} from "../actions";
import {showError} from "./Errors";

const layoutStyles = require("../Layout.scss");
const styles = require("./ProjectLink.scss");

export interface ProjectLinkProps { project: string; }
export interface ProjectLinkState { }

class ProjectLink extends React.Component<ProjectLinkProps&actionsInterface, ProjectLinkState> {
    render() {
        const project = this.props.project;
        return (<Link key={project} className={styles.container} to={"/project/" + project} onClick={()=>{
            this.props.dispatch.project.switchProject(project).then(()=>{
                if(this.props.appState.currentProject.questions.length!==0){
                    this.props.dispatch.question.switchQuestion(project, this.props.appState.currentProject.questions[0]);
                }
            }).catch((error)=>{
                if(error !== null){
                    showError(error.message);
                }
            })}}><span className={styles.linkContainer}>{project}</span></Link>);
    }
}

export default map<ProjectLinkProps>(ProjectLink);