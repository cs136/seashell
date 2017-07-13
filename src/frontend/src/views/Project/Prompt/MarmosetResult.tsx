import * as React from "react";
import {merge} from "ramda";
import * as Blueprint from "@blueprintjs/core";
import {map, actionsInterface} from "../../../actions";

interface MarmosetResultProps {
  result: any;
};

class MarmosetResult extends React.Component<MarmosetResultProps&actionsInterface, {}> {

  render() {
    const {result} = this.props;
    let message = (<div className="pt-callout">
        <strong>Loading Results...</strong>
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
    return(<div className="pt-dialog-body">
        {message}
    </div>);
  }
}

export default map<MarmosetResultProps>(MarmosetResult);
