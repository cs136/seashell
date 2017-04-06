import * as React from "react";
import {map, actionsInterface} from "../../actions";
import MonacoEditor from "./Editor";
import  Xterm  from "./Console";
import Loading from "./Loading";
import * as Draggable from "react-draggable"; // Both at the same time
import { merge } from "ramda";

const styles = require("./project.scss");

export interface FileProps { file: string; className?: string; };
export interface FileState { dirty: boolean; editorLastUpdated: number; }

class File extends React.Component<FileProps & actionsInterface, FileState> {
  editor: any;

  constructor(props: FileProps & actionsInterface) {
    super(props);
    this.state = {
        dirty: true,
        editorLastUpdated: -1
    };
    this.handleDrag = this.handleDrag.bind(this);
    this.stopDrag = this.stopDrag.bind(this);
    this.onResize = this.onResize.bind(this);
    this.editor = null;
  }
  onResize() {
    if (!("terminal" in this.refs) || this.editor == null) return; // ignore if not mounted properly yet
    const newHeight = (window.innerHeight - this.editor.domElement.getBoundingClientRect().top);
    const newHeightPx = newHeight + "px";
    this.editor.domElement.style.height = newHeightPx;
    (this.refs.resizeHandle as HTMLElement).style.height = newHeightPx;
    (this.refs.editorContainer as HTMLElement).style.height = newHeightPx;
    this.editor.domElement.style.flex = this.props.settings.editorRatio;
    (this.refs.terminal as Xterm).setFlex(1 - this.props.settings.editorRatio);
    (this.refs.terminal as Xterm).setHeight(newHeight);
    this.editor.layout();
    (this.refs.terminal as Xterm).updateLayout();
  }
  updateEditorOptions() {
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
    this.setState(merge(this.state, {editorLastUpdated: this.props.settings.updated}));
    if (this.editor) {
      this.editor.updateOptions(editorOptions);
      this.editor.getModel().updateOptions({tabSize: this.props.settings.tabWidth});
    }
  }
  updateConsoleOptions() {
    if (this.props.settings.theme) {
      (this.refs.terminal as Xterm).term.element.classList.add("xterm-theme-light");
      (this.refs.terminal as Xterm).term.element.classList.remove("xterm-theme-default");
    } else {
      (this.refs.terminal as Xterm).term.element.classList.remove("xterm-theme-light");
      (this.refs.terminal as Xterm).term.element.classList.add("xterm-theme-default");
    }
  }
  editorDidMount(editor: any, monaco: any) {
    this.editor = editor;
    this.onResize();
    editor.focus();
    this.updateEditorOptions();
  }
  onChange(newValue: string, e: any) {
    this.props.dispatch.file.updateFile(this.props.appState.currentProject.name, this.props.appState.currentProject.currentQuestion.currentFile.name, newValue);
  }
  componentDidUpdate() {
    if (this.state.editorLastUpdated !== this.props.settings.updated) {
      this.updateEditorOptions();
      this.updateConsoleOptions();
    }
  }
  componentWillUnmount() {
    window.removeEventListener("resize", this.onResize);
    this.editor = null;
  }
  componentDidMount() {
      window.removeEventListener("resize", this.onResize);
      this.onResize = this.onResize.bind(this);
      window.addEventListener("resize", this.onResize);
      this.onResize();
      this.updateConsoleOptions();
  }
  stopDrag(e: any) {
    const percent = e.clientX / window.innerWidth;
    this.props.dispatch.settings.updateEditorRatio(percent);
    this.onResize();
  }
  handleDrag(e: any) {
    const percent = e.clientX / window.innerWidth;
    this.editor.domElement.style.flex = percent;
    (this.refs.terminal as Xterm).setFlex(1 - percent);
  }
  render() {
    const loaderOptions = {
        url: "vs/loader.js",
        paths: {
            vs: "vs"
        }
    };
    const xtermOptions = {
        tabstopwidth: this.props.settings.tabWidth,
        cursorBlink: true,
    };
    const currentFile = this.props.appState.currentProject.currentQuestion.currentFile;
    if (this.props.file === currentFile.name) {
      return (<div className={styles.filePanel}>
        <div className={styles.editorContainer + " " + this.props.className} ref="editorContainer">
          <MonacoEditor
            value={currentFile.content} language="cpp"
            onChange={this.onChange.bind(this)}
            editorDidMount={this.editorDidMount.bind(this)} requireConfig={loaderOptions}/>
          <Draggable axis="x" handle="div" onDrag={this.handleDrag} onStop={this.stopDrag}>
            <div ref="resizeHandle" className={styles.resizeHandle} />
          </Draggable>
        <Xterm ref="terminal"
          className={this.props.settings.theme ? "xterm-wrapper-light" : "xterm-wrapper-default" }
          readOnly={this.props.appState.runState!==2} dispatch={this.props.dispatch}/>
      </div>
      </div>);
    }
    else
      return <Loading/>
  }
}
export default map<FileProps>(File);
