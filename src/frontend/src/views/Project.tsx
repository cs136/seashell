import * as React from 'react';
import {Tabs, TabList, TabPanel, Tab} from '@blueprintjs/core';
import {map, actionsInterface} from '../actions';
import MonacoEditor from 'react-monaco-editor';
import XTerm from 'react-xterm';

const styles = require<any>('./Project.scss');
const layoutStyles = require<any>('../Layout.scss');

export interface ProjectProps { title: string; routeParams: any; }
export interface ProjectState { open?: boolean; title?: string; }



class Project extends React.Component<ProjectProps&actionsInterface, ProjectState>{
  constructor(props: ProjectProps&actionsInterface){
    super(props);
  }
  editorDidMount(editor: any, monaco: any) {
    console.log('editorDidMount', editor);
    editor.getModel().updateOptions({tabSize: this.props.settings.tabWidth});
    editor.focus();
  }
  onChange(newValue: any, e: any) {
    console.log('onChange', newValue, e);
    this.props.dispatch.file.updateFile({content: newValue});
  }
  render(){
    const editorOptions={
      automaticLayout: false,
			cursorBlinking: 'phase',
			cursorStyle: 'line',
			parameterHints: true,
			readOnly: false,
			roundedSelection: true,
			scrollBeyondLastLine: false,
			scrollbar: {
			 vertical: 'visible',
			},
			selectOnLineNumbers: true,
			theme: (this.props.settings.theme) ? 'vs': 'vs-dark',
      wrappingColumn: 0,
      fontFamily: this.props.settings.font+", monospace",
      fontSize: this.props.settings.fontSize
    };
    const loaderOptions = {
      url: "dist/vs/loader.js",
      paths: {
        vs: "dist/vs"
      }
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
                  <MonacoEditor height="800" width="500" options={editorOptions} value={file.content} language="cpp" onChange={this.onChange.bind(this)} editorDidMount={this.editorDidMount.bind(this)}
                  requireConfig={loaderOptions}/>
                </TabPanel>))}
                <XTerm/>
            </Tabs>
          </TabPanel>))}
      </Tabs>
    </div>);
  } 
}
 
export default map<ProjectProps>(Project);
