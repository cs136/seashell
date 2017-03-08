import * as React from "react";
import {Tabs, TabList, TabPanel, Tab} from "@blueprintjs/core";
import {map, actionsInterface} from "../actions";
import MonacoEditor from "react-monaco-editor";
import Xterm from "./Console";

const styles = require<any>("./Project.scss");
const layoutStyles = require<any>("../Layout.scss");


export interface ProjectProps { title: string; routeParams: any; }
export interface ProjectState { open?: boolean; title?: string; editor?: any; }



class Project extends React.Component<ProjectProps&actionsInterface, ProjectState> {
  constructor(props: ProjectProps&actionsInterface) {
    super(props);
    this.onResize = this.onResize.bind(this);
    this.state = {editor: null};
  }
  onResize(){
    this.state.editor.domElement.style.height = (window.innerHeight - this.state.editor.domElement.getBoundingClientRect().top) + "px";
    this.state.editor.layout();
  }
  editorDidMount(editor: any, monaco: any) {
    editor.getModel().updateOptions({tabSize: this.props.settings.tabWidth});
    window.removeEventListener("resize", this.onResize);
    window.addEventListener("resize", this.onResize);
    this.state.editor = editor;
    this.onResize();
    editor.focus();
  }
  onChange(newValue: string, e: any) {
    this.props.dispatch.file.updateFile(newValue);
  }
  render() {
    const editorOptions = {
      automaticLayout: true,
      cursorBlinking: "phase",
      cursorStyle: "line",
      parameterHints: true,
      readOnly: false,
      roundedSelection: true,
      scrollBeyondLastLine: false,
      scrollbar: {
        vertical: "visible",
      },
      selectOnLineNumbers: true,
      theme: (this.props.settings.theme) ? "vs" : "vs-dark",
      wrappingColumn: 0,
      fontFamily: this.props.settings.font + ", monospace",
      fontSize: this.props.settings.fontSize
    };
    const loaderOptions = {
      url: "dist/vs/loader.js",
      paths: {
        vs: "dist/vs"
      }
    };
    const xtermOptions = {
      tabstopwidth: this.props.settings.tabWidth,
      cursorBlink: true,
    };
    const termWrapperStyle = {
      width: "50%",
      height: "50%"
    };
    const projectId = this.props.routeParams.id;
    const project = this.props.appState.currentProject;
    return (<div className={styles.fullWidth}>
        <Tabs initialSelectedTabIndex={1}>
          <TabList className={styles.tabList + " pt-large"}>
            <Tab isDisabled={true}><h5>{project.name}</h5></Tab>
            {project.questions.map((question) => (<Tab key={"tab-" + question}>{question}</Tab>))}
          </TabList>
          <TabPanel />
          {project.questions.map((question) => (
          <TabPanel className={styles.tabPanel} key={"question-" + question}>
            {question === project.currentQuestion.name ? (<Tabs>
              <TabList className={styles.tabList + " " + styles.fileTabList}>
                {project.currentQuestion.files.map((file) => (<Tab key={"file-tab-" + file}>{file}</Tab>))}
              </TabList>
              {project.currentQuestion.files.map((file) => (<TabPanel key={"file-" + file} className={styles.tabPanel}>
                <nav className={styles.actionbar + " pt-navbar pt-dark"}>
                  <div className="pt-navbar-group pt-align-left">
                    <button className="pt-button pt-minimal">Move/Rename</button>
                    <button className="pt-button pt-minimal">Copy</button>
                    <button className="pt-button pt-minimal">Delete</button>
                    <button className="pt-button pt-minimal">Set as <span className="pt-icon-standard pt-icon-play" />File</button>
                  </div>
                  <div className="pt-navbar-group pt-align-right">
                    <button className="pt-button pt-minimal">Test</button>
                    <button className="pt-button pt-minimal"><span className="pt-icon-standard pt-icon-play" />Run</button>
                  </div>
                </nav>
                {(file === project.currentQuestion.currentFile.name) ? (<MonacoEditor ref="editor" width="50%" options={editorOptions} value={project.currentQuestion.currentFile.content} language="cpp" onChange={this.onChange.bind(this)} editorDidMount={this.editorDidMount.bind(this)}
                  requireConfig={loaderOptions}/>) : null}
                </TabPanel>))}
            </Tabs>) : null}
          </TabPanel>))}
      </Tabs>
      <Xterm style={termWrapperStyle}/>
    </div>);
  }
}

export default map<ProjectProps>(Project);
