import * as React from "react";

import {merge} from "ramda";

import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";

export interface RenameWindowProps {questions: string[]; closefunc: Function; file: string};

class RenameWindow extends React.Component<RenameWindowProps&actionsInterface, {question: string; file: string, prevFile: string}>{
  constructor(props: RenameWindowProps&actionsInterface){
    super(props);
    this.state={
      question: this.props.questions[0],
      file: this.props.file,
      prevFile: this.props.file
    }
  }
  render(){
    return(<div>
      <p>Where would you like to rename/move this file?</p>
      <div><div className="pt-select pt-fill"><select id="question" value={this.state.question} onChange={(e) => this.setState(merge(this.state, {question: e.currentTarget.value}))}>
        {this.props.questions.map((question: string)=>(<option value={question}>{question}</option>))}
        </select></div>
        <input className="pt-input pt-fill" required type="text" value={this.state.file} 
        onBlur={()=>{
          if(this.state.file===""||this.state.file.includes("/")){
            this.setState(merge(this.state, {file: this.state.prevFile}));
          }
          else{
            this.setState(merge(this.state, {prevFile: this.state.file}));
          }
        }}
        onChange={(e => this.setState(merge(this.state, {file: e.currentTarget.value})))}/></div>
      <div className="pt-button-group">
        <button type="button" className="pt-button" onClick={() => {
                this.props.closefunc();
                }}>Cancel</button>
        <button type="button" className="pt-button pt-intent-primary" onClick={()=>{
          this.props.dispatch.file.renameFile(this.state.question+"/"+this.state.file);
          this.props.closefunc();
          }}>Rename/Move</button>
      </div>
    </div>
    );
  }
}

export default map<RenameWindowProps>(RenameWindow);