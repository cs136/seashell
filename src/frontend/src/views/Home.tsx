import * as React from "react";
import {map, actionsInterface} from "../actions";
import {Link} from "react-router";
import Navigation from "../partials/Navigation";
import ProjectLink from "../partials/ProjectLink";
const layoutStyles = require("../Layout.scss");
const styles = require("./Home.scss");

export interface HomeProps { title: string; }
export interface HomeState { open?: boolean; title?: string; }

class Home extends React.Component<HomeProps&actionsInterface, HomeState> {
  render() {
    const projects = this.props.appState.projects;
    return (<div>
      <Navigation navLeft={[
          <div className="pt-navbar-heading" key="project-name">My Projects</div>]}
          navRight={
            [<button className="pt-button" role="button" key="home-new-project"><span className="pt-icon-standard pt-icon-plus pt-align-left"></span>New Project</button>,
            <button className="pt-button" role="button" key="home-refresh"><span className="pt-icon-standard pt-icon-refresh pt-align-left"></span>Refresh</button>]
          }
           />
      <div className={styles.container}>
        <div className={styles.mainRow}>
          <div className={styles.column}>
            <h5>Assignments</h5>
            {projects.filter((project)=>project.toUpperCase().startsWith("A")).map((project) => <ProjectLink key={project} project={project} />)}
          </div>
          <div className={styles.column}>
            <h5>Tutorials</h5>
           {projects.filter((project)=>project.toUpperCase().startsWith("TUT")).map((project) => <ProjectLink key={project} project={project} />)}
          </div>
          <div className={styles.column}>
            <h5>Lectures</h5>
            {projects.filter((project)=>project.toUpperCase().startsWith("LEC")).map((project) => <ProjectLink key={project} project={project} />)}
          </div>
          <div className={styles.column}>
            <h5>Personal</h5>
            {projects.filter((project)=>!project.toUpperCase().startsWith("A")&&!project.toUpperCase().startsWith("TUT")&&!project.toUpperCase().startsWith("LEC")).map((project) => <ProjectLink key={project} project={project} />)}
          </div>
        </div>
        <div className={styles.clear} />
      </div>
    </div>);
  }
}

export default map(Home);
