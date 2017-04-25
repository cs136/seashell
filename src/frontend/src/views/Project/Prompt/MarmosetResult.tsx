import * as React from "react";

import {merge} from "ramda";

import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";

export interface MarmosetResultProps {};

class MarmosetResult extends React.Component<MarmosetResultProps&actionsInterface, {}> {
  constructor(props: MarmosetResultProps&actionsInterface) {
    super(props);
  }
  render() {
    return(<div className="pt-dialog-body">
        <div className="pt-callout">
            <strong>Loading Results...</strong> (2/5 tested)
        </div><br/>
        <div className="pt-callout pt-intent-success">
            <strong>Passed</strong> (5/5)
        </div><br/>
        <div className="pt-callout pt-intent-danger">
            <strong>Failed</strong> (2/5)
        </div>
    </div>
    );
  }
}

export default map<MarmosetResultProps>(MarmosetResult);
