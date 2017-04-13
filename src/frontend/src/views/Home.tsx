import * as React from "react";
import {map, actionsInterface} from "../actions";
import { Link } from "react-router-dom";
import { RouteComponentProps } from "react-router";
import Navigation from "../partials/Navigation";
import ProjectLink from "../partials/ProjectLink";
const layoutStyles = require("../Layout.scss");
const styles = require("./Home.scss");


export interface HomeProps extends RouteComponentProps<{}> { title: string; }
export interface HomeState { open?: boolean; title?: string; addProjectVisible: boolean}

class Home extends React.Component<HomeProps&actionsInterface, HomeState> {
  constructor(props: HomeProps&actionsInterface){
    super(props);
    this.state={
      addProjectVisible: false,
    }
  }
  toggleAddProj(){
    this.setState({addProjectVisible: !this.state.addProjectVisible});
  }
  render() {
    const projects = this.props.appState.projects;
    return (<div>
      <Navigation addProjectVisible={this.state.addProjectVisible} closeAddProj={this.toggleAddProj.bind(this)}
      navLeft={[
          <div className="pt-navbar-heading" key="project-name">My Projects</div>]}
          navRight={
            [<button className="pt-button" role="button" key="home-new-project" onClick={()=>this.toggleAddProj()}><span className="pt-icon-standard pt-icon-plus pt-align-left"></span>New Project</button>,
            <button className="pt-button" role="button" key="home-refresh" onClick={() => this.props.dispatch.project.getAllProjects()}><span className="pt-icon-standard pt-icon-refresh pt-align-left"></span>Refresh</button>]
          }
           />
      <div className={styles.container}>
        <div className={styles.mainRow}>
          <div className={styles.column}>
            <h5>Assignments</h5>{
              projects.filter((project) => {
                return project.name.toUpperCase().startsWith("A");
              }).map((project) => (
                <ProjectLink key={project.id} project={project} />
              ))
            }
          </div>
          <div className={styles.column}>
            <h5>Tutorials</h5>{
              projects.filter((project) => {
                return project.name.toUpperCase().startsWith("TUT");
              }).map((project) => (
                <ProjectLink key={project.id} project={project} />
              ))
            }
          </div>
          <div className={styles.column}>
            <h5>Lectures</h5>{
              projects.filter((project) => {
                return project.name.toUpperCase().startsWith("LEC")
                  || project.name.toUpperCase().startsWith("SEC");
              }).map((project) => (
                <ProjectLink key={project.id} project={project} />
              ))
            }
          </div>
          <div className={styles.column}>
            <h5>Personal</h5>{
              projects.filter((project) => {
                return !project.name.toUpperCase().startsWith("A")
                  && !project.name.toUpperCase().startsWith("TUT")
                  && !project.name.toUpperCase().startsWith("LEC")
                  && !project.name.toUpperCase().startsWith("SEC");
              }).map((project) => (
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
