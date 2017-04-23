import * as React from "react";
import {Link} from "react-router-dom";

import {map, actionsInterface} from "../actions";
import {showError} from "./Errors";
import * as S from "../helpers/Storage/Interface";

const layoutStyles = require("../Layout.scss");
const styles = require("./ProjectLink.scss");

export interface ProjectLinkProps { project: S.ProjectBrief; }
export interface ProjectLinkState { }

class ProjectLink extends React.Component<ProjectLinkProps&actionsInterface, ProjectLinkState> {
    render() {
        const project = this.props.project;
        return (<Link className={styles.container} to={"/project/" + project.id + "/" + project.name}>
            <span className={styles.linkContainer}>{project.name}</span></Link>);
    }
}

export default map<ProjectLinkProps>(ProjectLink);