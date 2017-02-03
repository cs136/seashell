import * as React from 'react';

import {evolve} from 'ramda';

import {Tag, Classes} from '@blueprintjs/core';
import {map, actionsInterface} from '../../actions';
import {settingsReducerState} from '../../reducers/settingsReducer';

export interface DialogProps { }
export interface DialogState { }




class HelpDialog extends React.Component<DialogProps, DialogState>{
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
export {HelpDialog}
