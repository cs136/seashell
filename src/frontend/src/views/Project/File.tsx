import * as React from "react";
import {map, actionsInterface} from "../../actions";
import MonacoEditor from "react-monaco-editor";
import Xterm from "./Console";
import Loading from "./Loading";
const styles = require<any>("./project.scss");

export interface FileProps { file: string; };
export interface FileState { editor: any; }

class Question extends React.Component<FileProps & actionsInterface, FileState> {
  constructor(props: FileProps & actionsInterface) {
    super(props);
    this.state = {
        editor: null
    };
  }
  onResize() {
    if (!("terminal" in this.refs) || this.state.editor == null) return; // ignore if not mounted properly yet
    const newHeight = (window.innerHeight - this.state.editor.domElement.getBoundingClientRect().top);
    this.state.editor.domElement.style.height = newHeight + "px";
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
    return (this.props.file === currentFile.name ? (<div className={styles.tabPanel}>
        <div className={styles.editorContainer}><MonacoEditor ref="editor" options={editorOptions} value={currentFile.content} language="cpp" onChange={this.onChange.bind(this)} editorDidMount={this.editorDidMount.bind(this)} requireConfig={loaderOptions}/><Xterm ref="terminal" readOnly={false}/></div>
    </div>) : <Loading />);
  }
}
export default map<FileProps>(Question);
