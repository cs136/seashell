import * as React from "react";
import {map, actionsInterface} from "../actions";
import { Link } from "react-router-dom";
import { RouteComponentProps } from "react-router";
import Navigation from "../partials/Navigation";
import ProjectLink from "../partials/ProjectLink";
const layoutStyles = require("../Layout.scss");
const styles = require("./Home.scss");


export interface HomeProps extends RouteComponentProps<{}> { title: string; }
export interface HomeState { open?: boolean; title?: string; }

class Home extends React.Component<HomeProps&actionsInterface, HomeState> {
  constructor(props: HomeProps&actionsInterface) {
    super(props);
  }

  render() {
    const projects = this.props.appState.projects;
    const assns = projects.filter((project) => project.name.toUpperCase().match(/^A[0-9]+$/));
    const tuts = projects.filter((project) => project.name.toUpperCase().match(/^TUT[0-9]+/));
    const lecs = projects.filter((project) => project.name.toUpperCase().match(/^[LS]EC[0-9]+/));
    const pers = projects.filter((project) => assns.indexOf(project) === -1
                                           && tuts.indexOf(project) === -1
                                           && lecs.indexOf(project) === -1);
    return (<div>
      <Navigation
          navLeft={[
          <div className="pt-navbar-heading" key="project-name">My Projects</div>]}
          navRight={
            [<button className="pt-button" role="button" key="home-new-project" onClick={this.props.dispatch.dialog.toggleAddProject}>
              <span className="pt-icon-standard pt-icon-plus pt-align-left"></span>
              New Project
            </button>,
            <button className="pt-button" role="button" key="home-refresh" onClick={this.props.dispatch.project.getAllProjects}>
              <span className="pt-icon-standard pt-icon-refresh pt-align-left"></span>
              Refresh
            </button>]
          }
           />
      <div className={styles.container}>
        <div className={styles.mainRow}>
          <div className={styles.column}>
            <h5>Assignments</h5>{
              assns.map((project) => (
                <ProjectLink key={project.id} project={project} />
              ))
            }
          </div>
          <div className={styles.column}>
            <h5>Tutorials</h5>{
              tuts.map((project) => (
                <ProjectLink key={project.id} project={project} />
              ))
            }
          </div>
          <div className={styles.column}>
            <h5>Lectures</h5>{
              lecs.map((project) => (
                <ProjectLink key={project.id} project={project} />
              ))
            }
          </div>
          <div className={styles.column}>
            <h5>Personal</h5>{
              pers.map((project) => (
                <ProjectLink key={project.id} project={project} />
              ))
            }
          </div>
        </div>
        <div className={styles.clear} />
      </div>
    </div>);
  }
}

export default map<HomeProps>(Home);
