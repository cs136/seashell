import * as React from "react";
import {Tabs, TabList, TabPanel, Tab} from "@blueprintjs/core";
import {map, actionsInterface} from "../actions";
import Xterm from "./Console";
import AceEditor from 'react-ace';
import * as brace from 'brace';
import 'brace/mode/c_cpp';
import 'brace/mode/scheme';
import 'brace/ext/searchbox';
import 'brace/theme/terminal';
import 'brace/theme/eclipse';
import 'brace/keybinding/vim';
import 'brace/keybinding/emacs';

const styles = require<any>("./Project.scss");
const layoutStyles = require<any>("../Layout.scss");


export interface ProjectProps { title: string; routeParams: any; }
export interface ProjectState { open?: boolean; title?: string; }



class Project extends React.Component<ProjectProps&actionsInterface, ProjectState> {
  constructor(props: ProjectProps&actionsInterface) {
    super(props);
  }
  onLoad(editor: any, content: string){
    editor.getSession().setValue(content);
  }
  onChange(newValue: string, e: any) {
    this.props.dispatch.file.updateFile(newValue);
  }
  render() {
    const xtermOptions={
      tabstopwidth: this.props.settings.tabWidth,
      cursorBlink: true,
    };
    const termWrapperStyle={
      width: "50%",
      height: "50%"
    };
    const projectId = this.props.routeParams.id;
    const project = this.props.appState.currentProject;
    return (<div className={layoutStyles.container}>
        <Tabs initialSelectedTabIndex={1}>
          <TabList className="pt-large">
            <Tab isDisabled={true}>{project.name}</Tab>
            {project.questions.map((question) => (<Tab key={"tab-" + question}>{question}</Tab>))}
          </TabList>
          <TabPanel />
          {project.questions.map((question) => (
          <TabPanel key={"question-" + question}>
            {question==project.currentQuestion.name?(<Tabs>
              <TabList>
                {project.currentQuestion.files.map((file) => (<Tab key={"file-tab-" + file}>{file}</Tab>))}</TabList>
                {project.currentQuestion.files.map((file) => (<TabPanel key={"file-" + file}>
                {(file===project.currentQuestion.currentFile.name)?(<div>
                  <h3>{project.currentQuestion.currentFile.name}</h3>
                  <AceEditor height="800" width="500" value={project.currentQuestion.currentFile.content} 
                  mode={(project.currentQuestion.currentFile.name.split(".").pop()==="c"||project.currentQuestion.currentFile.name.split(".").pop()==="c")? "c_cpp" :
                      ((project.currentQuestion.currentFile.name.split(".").pop()==="rkt")? "scheme" : "")} 
                  onChange={this.onChange.bind(this)}
                  theme={this.props.settings.theme ? "eclipse" : "terminal"}
                  keyboardHandler={(this.props.settings.editorMode) ? ((this.props.settings.editorMode===2)? "emacs" : "vim") : ""}
                  tabSize={this.props.settings.tabWidth} fontSize={this.props.settings.fontSize} setOptions={{fontFamily: this.props.settings.font+", monospace"}}
                  focus={true} wrapEnabled={true} onLoad={(editor: any)=>this.onLoad.bind(this)(editor, project.currentQuestion.currentFile.content)}/></div>):null}
                </TabPanel>))}
            </Tabs>):null}
          </TabPanel>))}
      </Tabs>
      <Xterm style={termWrapperStyle}/>
    </div>);
  }
}

export default map<ProjectProps>(Project);
