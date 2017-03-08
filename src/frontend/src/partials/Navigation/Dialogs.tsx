import * as React from "react";

import {evolve, merge} from "ramda";

import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import {settingsReducerState} from "../../reducers/settingsReducer";


export interface DialogProps { }
export interface DialogState { }
export interface SettingsDialogProps {closefunc: Function; }

export interface SettingsDialogState { font?: string; fontSize?: number; editorMode?: number; tabWidth?: number;  theme?: number; offlineMode?: number; }

class FontTextBox extends React.Component<{font: string, changeParentState: Function}, {font: string}> {
  constructor(props: {font: string, changeParentState: Function}) {
    super(props);
    this.state = {
      font: this.props.font
    };
  }
  render() {
    return(
      <input className="pt-input pt-fill" required type="text" value={this.state.font} onBlur={() => {
        if (this.state.font === "") {
          this.setState({font: this.props.font});
        }
        else {
          this.props.changeParentState(this.state.font);
        }
      }} onChange={(e => this.setState({font: e.target.value}))}/>
    );
  }
}

class FontSizeTextBox extends React.Component<{fontSize: number, changeParentState: Function}, {fontSize: string}> {
  constructor(props: {fontSize: number, changeParentState: Function}) {
    super(props);
    this.state = {
      fontSize: this.props.fontSize.toString()
    };
  }
  render() {
    return(
      <input className="pt-input pt-fill" required type="text" value={this.state.fontSize} onBlur={() => {
        if (isNaN(Number(this.state.fontSize)) || Number(this.state.fontSize) <= 0) {
          this.setState({fontSize: this.props.fontSize.toString()});
        } else {
          this.props.changeParentState(Number(this.state.fontSize));
        }
      }} onChange={(e => this.setState({fontSize: e.target.value}))}/>
    );
  }
}

class SettingsDialog extends React.Component<DialogProps&actionsInterface&SettingsDialogProps, DialogState&SettingsDialogState> {
  constructor(props: DialogProps&actionsInterface&SettingsDialogProps) {
    super(props);
    this.state = {
      font: this.props.settings.font,
      fontSize: this.props.settings.fontSize,
      editorMode: this.props.settings.editorMode,
      tabWidth: this.props.settings.tabWidth,
      theme: this.props.settings.theme,
      offlineMode: this.props.settings.offlineMode
    };
  }

  render() {
    return (<div className="pt-dialog-body">
          <label className="pt-label">
            Font: 
            <div className="pt-control-group"><FontTextBox font={this.state.font} changeParentState={(e: string) => this.setState.bind(this)(merge(this.state, {font: e}))}/></div>
          </label>
          <label className="pt-label">
            Font Size: 
              <div className="pt-control-group">
              <FontSizeTextBox fontSize={this.state.fontSize} changeParentState={(e: number) => this.setState.bind(this)(merge(this.state, {fontSize: e}))}/>
            </div>
          </label>
          <label className="pt-label">
            Editor mode:
              <div className="pt-control-group">
              <div className="pt-select pt-fill">
                <select id="editor_mode" value={String(this.state.editorMode)} onChange={(e) => this.setState(merge(this.state, {editorMode: Number(e.target.value)}))}>
                  <option value="0">Standard</option>
                  <option value="1">Vim</option>
                  <option value="2">Emacs</option>
                </select>
              </div>
            </div>
          </label>
          <label className="pt-label">
            Tab Width
              <div className="pt-control-group">
              <div className="pt-select pt-fill">
                <select id="tab_width" value={String(this.state.tabWidth)} onChange={(e) => this.setState(merge(this.state, {tabWidth: Number(e.target.value)}))}>
                  <option value="1">1</option>
                  <option value="2">2</option>
                  <option value="4">4</option>
                </select>
              </div>
            </div>
          </label>
          <label className="pt-label">
            Theme:
              <div className="pt-control-group">
              <div className="pt-select pt-fill">
              <select id="theme_style" value={String(this.state.theme)} onChange={(e) => this.setState(merge(this.state, {theme: Number(e.target.value)}))}>
                <option value="0">dark</option>
                <option value="1">light</option>
              </select>
              </div>
            </div>
          </label>
          <label className="pt-label">
            Offline Mode:<br/><br/>
              <div className="pt-control-group">
              <Blueprint.RadioGroup onChange={() => {}} selectedValue={String(this.state.offlineMode)}>
                <Blueprint.Radio label="Disabled" value="0" onClick={() => this.setState(merge(this.state, {offlineMode: 0}))}/>
                <Blueprint.Radio label="Enabled" value="1" onClick={() => this.setState(merge(this.state, {offlineMode: 1}))}/>
                <Blueprint.Radio label="Forced" value="2" onClick={() => this.setState(merge(this.state, {offlineMode: 2}))}/>
              </Blueprint.RadioGroup>

            </div>
          </label>
          <div className="pt-button-group">
            <button type="button" className="pt-button" onClick={() => {
                this.props.closefunc();
              }}>Cancel</button>
            <button type="button" className="pt-button pt-intent-primary" onClick={() => {
                this.props.dispatch.settings.updateSettings({
                font: this.state.font,
                fontSize: this.state.fontSize,
                editorMode: this.state.editorMode,
                tabWidth: this.state.tabWidth,
                theme: this.state.theme,
                offlineMode: this.state.offlineMode});
                this.props.closefunc();
              }}>Save
              </button>
          </div>
        </div>);

  }
}



class HelpDialog extends React.Component<DialogProps, DialogState> {
  render() {
    return (<div className="pt-dialog-body">
        <h4>Getting Started</h4>
        <p>See the <a target="_blank" href="docs/user.html">Seashell User Documentation</a>.
        The <a href="https://www.youtube.com/channel/UC6SqoYX4CAEZSHGDqImzd2Q">CS136 YouTube channel </a>
        has videos explaining the basics of Seashell and common errors you may encounter.</p>
        <h4>Reset Seashell</h4>
        <p>It often helps to reset your Seashell instance if it gets locked
        up in some way. To do this, <a href="">log in again</a>,
        and check the box labeled "Check to reset your Seashell instance."</p>
        <h4>Cluttered old projects?</h4>
        <p>If you have used Seashell in a previous term and want to clear all of
        your old projects, you can <a href="">archive all
        projects</a>.</p>
        <h4>Default Editor Shortcuts</h4>
        <p>These are shortcuts that will work when the editor window is focused.</p>
        <div className="pt-card pt-elevation-2">
          <p>
            <Blueprint.Tag>Ctrl-f or Cmd-f</Blueprint.Tag> Start new search.
          </p>
          <p>
            <Blueprint.Tag>Ctrl-g or Cmd-g</Blueprint.Tag> Go to next match.
          </p>
          <p>
            <Blueprint.Tag>Ctrl-G or Cmd-G</Blueprint.Tag> Go to previous match.
          </p>
          <p>
            <Blueprint.Tag>Ctrl-z or Cmd-z</Blueprint.Tag> Undo.
          </p>
          <p>
            <Blueprint.Tag>Ctrl-y or Cmd-y</Blueprint.Tag> Redo.
          </p>
          <p>
            <Blueprint.Tag>Ctrl-i</Blueprint.Tag> Indent all.
          </p>
          <p>
            <Blueprint.Tag>Ctrl-Enter</Blueprint.Tag> Fullscreen Editor.
          </p>
          <p>
            <Blueprint.Tag>Ctrl-, and Ctrl-.</Blueprint.Tag> Change font size.
          </p>
          <p>
            <Blueprint.Tag>Highlight Lines, Tab</Blueprint.Tag> Block Indent.
          </p>
          <p>
            <Blueprint.Tag>Highlight Line, Shift-Tab</Blueprint.Tag> Block Unindent.
          </p>
        </div>
        <br/><br/>
        <h4>Giving Feedback</h4>
        <p>Any feedback you may have on Seashell, especially bugs you have encountered, can be reported
        using <a href="https://gitreports.com/issue/cs136/seashell">Seashell"s Git Reports page</a>.
        Please include your UW userid and full name.</p>
      </div>);
  }
}
export default map<SettingsDialogProps>(SettingsDialog);
export {HelpDialog}
