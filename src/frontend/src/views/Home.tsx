import * as React from 'react';
import {map, actionsInterface} from '../actions';
import {projectReducerState} from '../reducers/projectReducer';
import {Link} from 'react-router';

const layoutStyles = require<any>('../Layout.scss');
const styles = require<any>('./Home.scss');

export interface HomeProps { title: string; }
export interface HomeState { open?: boolean; title?: string; }

class Home extends React.Component<HomeProps&actionsInterface, HomeState>{
  render(){
    const projects:projectReducerState = this.props.projectList.projects;
    return (<div className={layoutStyles.container}>
      <div className="pt-button-group">
        <a className="pt-button" role="button"><span className="pt-icon-standard pt-icon-plus pt-align-left"></span>New Project</a>
        <a className="pt-button" role="button"><span className="pt-icon-standard pt-icon-refresh pt-align-left"></span>Refresh</a>
      </div>
      <div className={styles.mainRow}>
        <div className={styles.column}>
          <h5>Assignments</h5>
          {Object.keys(projects).map((key)=>(
              <Link className={"pt-card pt-elevation-1 pt-interactive " + styles.card} to={"project/"+key}>{projects[key].name}</Link>
          ))}
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
    </div>);
  }
}

export default map(Home);
