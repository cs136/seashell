import * as React from 'react';

import {Tag, Classes} from '@blueprintjs/core';

export interface DialogProps { }
export interface DialogState { }

export class SettingsDialog extends React.Component<DialogProps, DialogState>{
  render(){
    <div class="modal-header">
      <button type="button" class="close" ng-click="$dismiss()" aria-hidden="true">&times;</button>
      <h4 class="modal-title">
        <span class="glyphicon glyphicon-cog"></span>
        Settings</h4>
      </div>
      <form name="settingsForm" ng-submit="settingsForm.$valid && saveSettings()" novalidate>
        <div class="modal-body">
          <div class="form-group"
          ng-class="{'has-error': settingsForm.editor_font_name.$invalid}">
          <label for=this.props.font>Font:</label>
          <input required list="fonts" name="editor_font_name" id="editor_font_name" ng-model="temp.font" class="form-control"
          placeholder="(double click/tap to list options -- or enter your own)"/>
          <datalist id="fonts">
            <option value="Courier New">
              <option value="Consolas">
                <option value="Monaco">
                  <option value="Menlo">
                  </datalist>
                  <span class="help-block" ng-show="settingsForm.editor_font_name.$invalid">Required</span>
                </div>

                <div class="form-group"
                ng-class="{'has-error': settingsForm.font_size.$invalid}">
                <label for=this.props.fontSize>Font size:</label>
                <input type="number" required name="font_size" ng-model="temp.font_size" id="editor_font" class="form-control" placeholder="(in points)">
                <span class="help-block" ng-show="settingsForm.editor_font.$invalid">Required, Must be a number</span>
              </div>

              <div class="form-group">
                <label for="editor_mode">Editor mode:</label>
                <select id="editor_mode" ng-model=this.props.editorMode class="form-control">
                  <option value="standard" selected>Standard</option>
                  <option value="vim">Vim</option>
                  <option value="emacs">Emacs</option>
                </select>
              </div>

              <div class="form-group">
                <label for="tab_width">Tab Width:</label>
                <select id="tab_width" ng-model=this.props.tabWidth class="form-control">
                  <option value="1">1</option>
                  <option value="2">2</option>
                  <option value="4" selected>4</option>
                </select>
              </div>

              <div class="form-group">
                <label for="theme_style">Theme:</label>
                <select id="theme_style" ng-model=this.props.tabWidth class="form-control">
                  <option value="light" selected>light</option>
                  <option value="dark">dark</option>
                </select>
              </div>
              <div class="form-group">
                <label>Offline mode: </label>
                <label class="radio-inline">
                  <input name="offline-mode" type="radio" ng-model=this.props.offlineMode ng-value="0">
                  Disabled
                </label>
                <label class="radio-inline">
                  <input name="offline-mode" type="radio" ng-model="temp.offline_mode" ng-value="1">
                  Enabled
                </label>
                <label class="radio-inline">
                  <input name="offline-mode" type="radio" ng-model="temp.offline_mode" ng-value="2">
                  Forced
                </label>
              </div>
              <div class="modal-footer">
                <button type="button" class="btn btn-default" ng-click="$dismiss()">Cancel</button>
                <input type="submit" name="submit" value="Save" class="btn btn-primary"></input>
              </div>
            </form>

  }
}

export class HelpDialog extends React.Component<DialogProps, DialogState>{
  render(){
    return (<div className="pt-dialog-body">
        <h4>Getting Started</h4>
        <p>See the <a target="_blank" href="docs/user.html">Seashell User Documentation</a>.
        The <a href="https://www.youtube.com/channel/UC6SqoYX4CAEZSHGDqImzd2Q">CS136 YouTube channel</a>
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
            <Tag>Ctrl-f or Cmd-f</Tag> Start new search.
          </p>
          <p>
            <Tag>Ctrl-g or Cmd-g</Tag> Go to next match.
          </p>
          <p>
            <Tag>Ctrl-G or Cmd-G</Tag> Go to previous match.
          </p>
          <p>
            <Tag>Ctrl-z or Cmd-z</Tag> Undo.
          </p>
          <p>
            <Tag>Ctrl-y or Cmd-y</Tag> Redo.
          </p>
          <p>
            <Tag>Ctrl-i</Tag> Indent all.
          </p>
          <p>
            <Tag>Ctrl-Enter</Tag> Fullscreen Editor.
          </p>
          <p>
            <Tag>Ctrl-, and Ctrl-.</Tag> Change font size.
          </p>
          <p>
            <Tag>Highlight Lines, Tab</Tag> Block Indent.
          </p>
          <p>
            <Tag>Highlight Line, Shift-Tab</Tag> Block Unindent.
          </p>
        </div>
        <br/><br/>
        <h4>Giving Feedback</h4>
        <p>Any feedback you may have on Seashell, especially bugs you have encountered, can be reported
        using <a href="https://gitreports.com/issue/cs136/seashell">Seashell's Git Reports page</a>.
        Please include your UW userid and full name.</p>
      </div>);
  }
}
