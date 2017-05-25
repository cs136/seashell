import * as React from "react";
import {map, actionsInterface} from "../../../actions";
import {showError} from "../../../partials/Errors";
import Prompt from "./Prompt";
import Confirm from "./Confirm";
import * as SR from "../../../reducers/appStateReducer";
import {Services} from "../../../helpers/Services";
import * as R from "ramda";
import {RouterProps, withRouter} from "react-router";

interface DeleteProps {
  closefunc: Function;
};

class DeleteProject extends React.Component<DeleteProps&actionsInterface&RouterProps, {}> {

  constructor(props: any) {
    super(props);
  }

  private async submitForm(): Promise<void> {
    const proj = this.props.appState.currentProject as SR.appStateReducerProjectState;
    await Services.storage().deleteProject(proj.id);
    const projs = this.props.appState.projects;
    this.props.appState.projects = projs.filter((p) => p.id !== proj.id, projs);
    this.props.history.push("/");
  }

  render() {
    const proj = this.props.appState.currentProject as SR.appStateReducerProjectState;
    return (<Confirm submitText="Delete" bodyText={`Are you sure you want to delete ${proj.name}?`}
        submitfunc={() => this.submitForm()} closefunc={this.props.closefunc} />);
  }
}

export default map<DeleteProps>(withRouter(DeleteProject));
