import * as React from "react";
import {map, actionsInterface} from "../../../actions";
import MonacoEditor from "./Editor";
import Console from "./Console";
import Loading from "../Loading";
import {CompilerDiagnostic, FlagMask} from "../../../helpers/Services";
import * as Draggable from "react-draggable"; // Both at the same time
import { merge } from "ramda";

const styles = require("../Project.scss");

export interface DisplayProps { className?: string; };
export interface DisplayState { editorLastUpdated: number; }

class Display extends React.Component<DisplayProps & actionsInterface, DisplayState> {
  editor: any;
  monaco: any;

  constructor(props: DisplayProps & actionsInterface) {
    super(props);
    this.state = {
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
    (this.refs.terminal as Console).setFlex(1 - this.props.settings.editorRatio);
    (this.refs.terminal as Console).setHeight(newHeight);
    this.editor.layout();
    (this.refs.terminal as Console).updateLayout();
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
    if (!this.refs.terminal)
      return;
    if (this.props.settings.theme) {
      (this.refs.terminal as Console).term.element.classList.add("xterm-theme-light");
      (this.refs.terminal as Console).term.element.classList.remove("xterm-theme-default");
    } else {
      (this.refs.terminal as Console).term.element.classList.remove("xterm-theme-light");
      (this.refs.terminal as Console).term.element.classList.add("xterm-theme-default");
    }
  }
  editorDidMount(editor: any, monaco: any) {
    this.editor = editor;
    this.monaco = monaco;
    this.onResize();
    editor.focus();
    this.updateEditorOptions();
  }
  onChange(newValue: string, e: any) {
    let state = this.props.appState;
    if (state.currentProject &&
        state.currentProject.currentQuestion &&
        state.currentProject.currentQuestion.currentFile)
      this.props.dispatch.file.updateFile(state.currentProject.currentQuestion.currentFile, newValue);
    else
      throw new Error("Updating undefined file?!");
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
    (this.refs.terminal as Console).setFlex(1 - percent);
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
    let state = this.props.appState;
    if (state.currentProject &&
        state.currentProject.currentQuestion &&
        state.currentProject.currentQuestion.currentFile) {
      const currentQuestion = state.currentProject.currentQuestion;
      const currentFile = state.currentProject.currentQuestion.currentFile;
      const fileDiags = currentQuestion.diags.filter((d: CompilerDiagnostic) => {
        return d.file === currentFile.name;
      });
      const lang = currentFile.extension() === "rkt" ? "racket" : "cpp";
      return (<div className={styles.filePanel}>
        <div className={styles.editorContainer + " " + this.props.className} ref="editorContainer">
          <MonacoEditor
            dirty = {!!currentFile.unwrittenContent}
            value = {(currentFile.contents === false ||
                     currentFile.contents === undefined) ? "Unavailable in browser!" :
                     currentFile.contents.contents}
            language = {lang}
            diags = {currentQuestion.diags}
            onChange = {this.onChange.bind(this)}
            editorDidMount = {this.editorDidMount.bind(this)} requireConfig={loaderOptions}
            readOnly = {!currentFile.contents || currentFile.hasFlag(FlagMask.READONLY)}
            options = {{lineNumbersMinChars: 2,
                        minimap: {enabled: false},
                        rulers: [0]}} />
          <Draggable axis="x" handle="div" onDrag={this.handleDrag} onStop={this.stopDrag}>
            <div ref="resizeHandle" className={styles.resizeHandle} />
          </Draggable>
          <Console ref="terminal"
            className={this.props.settings.theme ? "xterm-wrapper-light" : "xterm-wrapper-default"}
            readOnly={this.props.appState.runState !== 2} dispatch={this.props.dispatch}
            consoleText={(this.props.appState.currentProject && this.props.appState.currentProject.consoleText) ? this.props.appState.currentProject.consoleText : ""}/>
        </div>
      </div>);
    } else
      return <Loading/>;
  }
}

export default map<DisplayProps>(Display);
