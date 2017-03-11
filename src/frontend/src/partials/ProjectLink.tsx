import * as React from "react";
import {Link} from "react-router";

const layoutStyles = require<any>("../Layout.scss");
const styles = require<any>("./ProjectLink.scss");

export interface ProjectLinkProps { project: string; }
export interface ProjectLinkState { }

export default class ProjectLink extends React.Component<ProjectLinkProps, ProjectLinkState> {
    render() {
        const project = this.props.project;
        return (<Link key={project} className={styles.container} to={"/project/" + project}><span className={styles.linkContainer}>{project}</span></Link>);
    }
}
