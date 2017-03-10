import * as React from "react";
import {map, actionsInterface} from "../actions";
import {Link} from "react-router";
import ProjectLink from "../partials/ProjectLink";
const layoutStyles = require<any>("../Layout.scss");
const styles = require<any>("./Home.scss");

export interface HomeProps { title: string; }
export interface HomeState { open?: boolean; title?: string; }

class Home extends React.Component<HomeProps&actionsInterface, HomeState> {
  render() {
    const projects = this.props.appState.projects;
    return (<div className={styles.container}>
      <h3 className={styles.title}>My Projects</h3>
      <div className="pt-button-group">
        <a className="pt-button" role="button"><span className="pt-icon-standard pt-icon-plus pt-align-left"></span>New Project</a>
        <a className="pt-button" role="button"><span className="pt-icon-standard pt-icon-refresh pt-align-left"></span>Refresh</a>
      </div>
      <div className={styles.mainRow}>
        <div className={styles.column}>
          <h5>Assignments</h5>
          {projects.map((project) => (<ProjectLink project={project} />))}
        </div>
        <div className={styles.column}>
          <h5>Tutorials</h5>
        </div>
        <div className={styles.column}>
          <h5>Lectures</h5>
        </div>
        <div className={styles.column}>
          <h5>Personal</h5>
        </div>
      </div>
      <div className={styles.clear} />
    </div>);
  }
}

export default map(Home);
