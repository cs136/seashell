import * as React from "react";
import {merge} from "ramda";
import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import {withRouter, RouteComponentProps} from "react-router";
import {Project} from "../../helpers/Services";

interface AddProjectWindowProps extends RouteComponentProps<{}> {
  closefunc: Function;
  history: any;
}

interface AddProjectWindowState {
  proj: string;
  prevProj: string;
}

class AddProjectWindow extends React.Component<AddProjectWindowProps&actionsInterface, AddProjectWindowState> {

  constructor(props: AddProjectWindowProps&actionsInterface) {
    super(props);
    this.state = {
      proj: "",
      prevProj: ""
    };
  }

  render() {
    return (<div className="pt-dialog-body">
      <p>What would you like to call your new project?</p>
      <div>
        <input className="pt-input pt-fill" required type="text" value={this.state.proj}
        onBlur={() => {
          if (this.state.proj === "" || this.state.proj.includes("/")) {
            this.setState(merge(this.state, {file: this.state.prevProj}));
          }
          else {
            this.setState(merge(this.state, {prevFile: this.state.proj}));
          }
        }}
        onChange={(e => this.setState(merge(this.state, {proj: e.currentTarget.value})))}/></div>
      <div className="pt-button-group">
        <button type="button" className="pt-button" onClick={() => {
                this.props.closefunc();
                }}>Cancel</button>
        <button type="button" className="pt-button pt-intent-primary" disabled = {this.state.proj === ""} onClick={() => {
          this.props.dispatch.project.addProject(this.state.proj).then((proj: Project) =>
            this.props.dispatch.project.getAllProjects().then(() => {
              this.props.closefunc();
              this.props.history.push(`/project/${proj.id}/${proj.name}`);
            }));
          }}>Add Project</button>
      </div>
    </div>
    );
  }
}

export default withRouter<{closefunc: Function}>(map<AddProjectWindowProps>(AddProjectWindow));
