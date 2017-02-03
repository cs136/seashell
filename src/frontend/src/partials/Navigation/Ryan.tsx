import * as React from 'react';

import {evolve} from 'ramda';

import {Tag, Classes} from '@blueprintjs/core';
import {map, actionsInterface} from '../../actions';
import {settingsReducerState} from '../../reducers/settingsReducer';

export interface DialogProps { }
export interface DialogState { }

export interface SettingsDialogState { settings: settingsReducerState }

class SettingsDialog extends React.Component<DialogProps&actionsInterface, DialogState&SettingsDialogState>{
  constructor(){
    super();
    this.state = {
      settings: this.props.settings
    };
  }
  evolvestate(settings:settingsReducerState){
    this.setState({settings: evolve(this.state.settings, settings)});
  }
  render(){
    return (<div className="pt-dialog-body">
          <div className="form-group"
          ng-className="{'has-error': settingsForm.editor_font_name.$invalid}">
          <label> Font:</label>
          <input required type="text" value={this.state.settings.font} onChange={(e)=>this.evolvestate({font: e.target.value})}/>

          <span className="help-block" ng-show="settingsForm.editor_font_name.$invalid">Required</span>
          </div>

                <div className="form-group"
                ng-className="{'has-error': settingsForm.font_size.$invalid}">
                <label>Font size:</label>
                <input required type="number" value={this.state.settings.fontSize} onChange={(e)=>{
                  if(Number(e.target.value)>0){
                    this.evolvestate({fontSize: Number(e.target.value)});
                  }
                }}/>
                <span className="help-block" ng-show="settingsForm.editor_font.$invalid">Required, Must be a number</span>
              </div>

              <div className="form-group">
                <label for="editor_mode">Editor mode:</label>
                <select id="editor_mode" ng-model=this.props.editorMode className="form-control">
                  <option value="standard" selected>Standard</option>
                  <option value="vim">Vim</option>
                  <option value="emacs">Emacs</option>
                </select>
              </div>

              <div className="form-group">
                <label for="tab_width">Tab Width:</label>
                <select id="tab_width" ng-model=this.props.tabWidth className="form-control">
                  <option value="1">1</option>
                  <option value="2">2</option>
                  <option value="4" selected>4</option>
                </select>
              </div>

              <div className="form-group">
                <label for="theme_style">Theme:</label>
                <select id="theme_style" ng-model=this.props.tabWidth className="form-control">
                  <option value="light" selected>light</option>
                  <option value="dark">dark</option>
                </select>
              </div>
              <div className="form-group">
                <label>Offline mode: </label>
                <label className="radio-inline">
                  <input name="offline-mode" type="radio" ng-model=this.props.offlineMode ng-value="0">
                  Disabled
                </label>
                <label className="radio-inline">
                  <input name="offline-mode" type="radio" ng-model="temp.offline_mode" ng-value="1">
                  Enabled
                </label>
                <label className="radio-inline">
                  <input name="offline-mode" type="radio" ng-model="temp.offline_mode" ng-value="2">
                  Forced
                </label>
              </div></div></div>);

  }
}