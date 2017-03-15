import * as React from "react";
import {map, actionsInterface} from "../../actions";
import {Link} from "react-router";
import * as Blueprint from "@blueprintjs/core";

 

class ErrorList extends React.Component<actionsInterface, {}>{
    render(){
        const titleStyle={float: "left"};
        return (
            <div className="pt-dialog-body">
                {this.props.appState.errors.map((error, idx)=>
                    <div className="pt-card pt-elevation-2">
                        <div><div style={{float: "left"}}>{error.title}</div><div className="pt-align-right"><span className="pt-icon-standard pt-icon-cross" onClick={()=>this.props.dispatch.app.removeError(idx)}/></div></div>
                        <div>{error.body}</div></div>)}
            </div>
        );
    }
}

export default map(ErrorList);