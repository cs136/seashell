import * as React from 'react';
import {Tabs, TabList, TabPanel, Tab} from '@blueprintjs/core';
import {map, actionsInterface} from '../actions';
import * as CodeMirror from 'react-codemirror'; 
require('codemirror/mode/clike/clike');
const styles = require<any>('./Project.scss');
const layoutStyles = require<any>('../Layout.scss');

export interface ProjectProps { title: string; routeParams: any; }
export interface ProjectState { open?: boolean; title?: string; }

class Project extends React.Component<ProjectProps&actionsInterface, ProjectState>{
  render(){
    const codeMirrorOptions = {
      lineNumbers: true,
      mode: 'clike'
    };
    const projectId = this.props.routeParams.id;
    const project = this.props.projectList.projects.filter((project)=>(project.id==projectId))[0];
    return (<div className={layoutStyles.container}>
        <Tabs initialSelectedTabIndex={1}>
          <TabList className="pt-large">
            <Tab isDisabled={true}>{project.name}</Tab>
            {project.questions.map((question) => (<Tab key={"tab-"+question.name}>{question.name}</Tab>))}
          </TabList>
          <TabPanel/>
          {project.questions.map((question) => (<TabPanel key={"question-"+question.name}>
            <Tabs>
              <TabList>
                {question.files.map((file)=>(<Tab key={"file-tab-"+file.name}>{file.name}</Tab>))}</TabList>
                {question.files.map((file)=>(<TabPanel key={"file-"+file.name}>
                  <h3>{file.name}</h3>
                  <CodeMirror options={codeMirrorOptions} value={file.content}/>
                </TabPanel>))}
            </Tabs>
          </TabPanel>))}
      </Tabs>
    </div>);
  } 
}
 
export default map(Project);
