import * as React from 'react';
import {map, actionsInterface} from '../actions';
import Layout from '../Layout';

export interface AssignmentProps { title: string; routeParams: any; }
export interface AssignmentState { open?: boolean; title?: string; }

class Assignment extends React.Component<AssignmentProps&actionsInterface, AssignmentState>{
  render(){
    const assignment = this.props.projectList.projects[Number(this.props.routeParams.id)];
    return (<div>
        <h1>{assignment.name}</h1>
    </div>);
  }
}
 
export default map(Assignment);
