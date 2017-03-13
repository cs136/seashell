import * as React from "react";
import {map, actionsInterface} from "../../actions";
import MonacoEditor from "react-monaco-editor";
import Xterm from "./Console";
import Loading from "./Loading";
import * as Draggable from "react-draggable"; // Both at the same time

const styles = require<any>("./project.scss");

export interface FileProps { file: string; className?: string; };
export interface FileState { editor: any; }

class File extends React.Component<FileProps & actionsInterface, FileState> {
  constructor(props: FileProps & actionsInterface) {
    super(props);
    this.state = {
        editor: null
    };
    this.handleDrag = this.handleDrag.bind(this);
    this.stopDrag = this.stopDrag.bind(this);
    this.onResize = this.onResize.bind(this);
  }
  onResize() {
    if (!("terminal" in this.refs) || this.state.editor == null) return; // ignore if not mounted properly yet
    const newHeight = (window.innerHeight - this.state.editor.domElement.getBoundingClientRect().top);
    const newHeightPx = newHeight + "px";
    this.state.editor.domElement.style.height = newHeightPx;
    (this.refs.resizeHandle as HTMLElement).style.height = newHeightPx;
    (this.refs.editorContainer as HTMLElement).style.height = newHeightPx;
    this.state.editor.domElement.style.flex = this.props.settings.editorRatio;
    (this.refs.terminal as Xterm).setFlex(1 - this.props.settings.editorRatio);
    (this.refs.terminal as Xterm).setHeight(newHeight);
    this.state.editor.layout();
    (this.refs.terminal as Xterm).updateLayout();
  }
  editorDidMount(editor: any, monaco: any) {
    editor.getModel().updateOptions({tabSize: this.props.settings.tabWidth});
    this.state.editor = editor;
    this.onResize();
    editor.focus();
  }
  onChange(newValue: string, e: any) {
    this.props.dispatch.file.updateFile(newValue);
  }
  componentWillUnmount() {
    window.removeEventListener("resize", this.onResize);
  }
  componentDidMount() {
    this.onResize = this.onResize.bind(this);
    window.addEventListener("resize", this.onResize);
    this.onResize();
  }
  stopDrag(e: any) {
    const percent = e.clientX / window.innerWidth;
    this.props.dispatch.settings.updateEditorRatio(percent);
  }
  handleDrag(e: any) {
    const percent = e.clientX / window.innerWidth;
    this.state.editor.domElement.style.flex = percent;
    (this.refs.terminal as Xterm).setFlex(1 - percent);
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
    const currentFile = this.props.appState.currentProject.currentQuestion.currentFile;
    return (this.props.file === currentFile.name ? (<div className={styles.filePanel}>
        <div className={styles.editorContainer + " " + this.props.className} ref="editorContainer"><MonacoEditor ref="editor" options={editorOptions} value={currentFile.content} language="cpp" onChange={this.onChange.bind(this)} editorDidMount={this.editorDidMount.bind(this)} requireConfig={loaderOptions}/><Draggable axis="x" handle="div" onDrag={this.handleDrag} onStop={this.stopDrag}>
          <div ref="resizeHandle" className={styles.resizeHandle} />
      </Draggable><Xterm ref="terminal" readOnly={false}/></div>
    </div>) : <Loading />);
  }
}
export default map<FileProps>(File);
