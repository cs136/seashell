import * as React from "react";
import * as R from "ramda";
import {Contents, ContentsID} from "../../../helpers/Storage/Interface";
import {dateString} from "../../../helpers/utils";
import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";
import {showError} from "../../../partials/Errors";
import Prompt from "./Prompt";
const styles = require("../Project.scss");

interface ConflictProps {
  closefunc: Function;
}

interface ConflictState {
  selectedContents: Contents;
  selectedID: ContentsID;
  disabled: boolean;
}

class Conflict extends React.Component<ConflictProps&actionsInterface, ConflictState> {
  private filename: string;

  constructor(props: ConflictProps&actionsInterface) {
    super(props);
    if (this.props.appState.conflictContents.length <= 1) {
      throw new Error("Opening Conflict window when no conflict exists.");
    }
    this.state = {
      selectedContents: this.props.appState.conflictContents[0],
      selectedID: this.props.appState.conflictContents[0].id,
      disabled: false
    };
    this.filename = this.props.appState.conflictContents[0].filename;
  }

  private submitForm(): Promise<void> {
    return this.props.dispatch.file.resolveConflict(this.state.selectedContents);
  }

  render() {
    return (<Prompt submitMessage="Select this version" submitfunc={() => this.submitForm()}
        closefunc={this.props.closefunc} disable={(val: boolean) =>
          this.setState(R.merge(this.state, {disabled: val}))}>
      <p>{`A merge conflict occurred on ${this.filename} while syncing. Choose a version below you want to keep:`}</p>
      <div className="pt-form-group">
        <div className="pt-select pt-fill">
          <select value={this.state.selectedID} disabled={this.state.disabled} onChange={e => {
              let sel = this.props.appState.conflictContents.find((c: Contents) => c.id === e.currentTarget.value);
              this.setState(R.merge(this.state, {selectedID: e.currentTarget.value, selectedContents: sel}));
            }}>
            {this.props.appState.conflictContents.map((conts: Contents) =>
              (<option key={conts.id} value={conts.id}>{dateString(conts.time)}</option>))}
          </select>
        </div>
      </div>
      <div className="pt-form-group">
        <textarea className={`pt-input pt-fill ${styles.conflictText}`} readOnly
          value={this.state.selectedContents.contents} />
      </div>
    </Prompt>
    );
  }
}

export default map<ConflictProps>(Conflict);
