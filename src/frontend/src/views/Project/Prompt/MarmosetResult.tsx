import * as React from "react";
import {merge} from "ramda";
import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";
import {showError} from "../../../partials/Errors";
import * as S from "../../../helpers/Services";

interface MarmosetResultProps {
  result: any;
};

interface MarmosetResultState {
  disabled: boolean;
  marmProject: string;
  defaultMessage: string;
}

class MarmosetResult extends React.Component<MarmosetResultProps&actionsInterface, MarmosetResultState> {
  pid: S.ProjectID;
  pname: string;
  question: string;
  matchedProject: string;

  constructor(props: MarmosetResultProps&actionsInterface) {
    super(props);
    if (this.props.appState.currentProject
        && this.props.appState.currentProject.currentQuestion) {
      this.pid = this.props.appState.currentProject.id;
      this.pname = this.props.appState.currentProject.name;
      this.question = this.props.appState.currentProject.currentQuestion.name;
      const mproj = (this.props.appState.marmosetProjects || []).find((proj: S.MarmosetProject) =>
        `${this.pname}${this.question}`.toUpperCase() === proj.project);
      if (mproj) {
        this.matchedProject = mproj.project;
      }
      this.state = {
        disabled: false,
        marmProject: mproj === undefined ? "" : mproj.project,
        defaultMessage: "No results yet."
      };
    } else {
      throw new Error("Opening MarmosetResult prompt without a question selected.");
    }
  }

  private submit() {
    this.setState(merge(this.state, {disabled: true}));
    return this.props.dispatch.question.marmosetSubmit(this.pid, this.question, this.state.marmProject)
      .then(() => this.setState(merge(this.state, {disabled: false,
        defaultMessage: "Submitted! Waiting for results."})));
  }

  render() {
    const result = this.props.result;
    let message = (<div className="pt-callout">
        <strong>{this.state.defaultMessage}</strong>
    </div>);
    if (result !== null) {
      if (result.passed === result.points) {
        message = (<div className="pt-callout pt-intent-success">
          <strong>Passed</strong> ({result.passed}/{result.points})
        </div>);
      } else {
        message = (<div className="pt-callout pt-intent-danger">
          <strong>Failed</strong> ({result.passed}/{result.points}) <br/> 
          {result.long}
        </div>);
      }
    }
    return (<div className="pt-dialog-body">
      {message}
      <select className="pt-select" disabled={this.state.disabled}
          value={this.state.marmProject} onChange={e =>
            this.setState(merge(this.state, {marmProject: e.currentTarget.value}))}>
        {(this.props.appState.marmosetProjects || []).map((proj: S.MarmosetProject) =>
          (<option value={proj.project}>{proj.project}</option>))}
      </select>
      <button type="button" className="pt-button" onClick={() => this.submit()}>
        Submit to Marmoset
      </button>
    </div>);
  }
}

export default map<MarmosetResultProps>(MarmosetResult);
