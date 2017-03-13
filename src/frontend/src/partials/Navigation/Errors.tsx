import * as React from "react";
import {map, actionsInterface} from "../../actions";
import {Link} from "react-router";
import * as Blueprint from "@blueprintjs/core";

 

class ErrorList extends React.Component<actionsInterface, {}>{
    render(){
        return (
            <div>
                {this.props.appState.errors.map((error, idx)=>
                    <div className="pt-card pt-elevation-2">
                        <div><div className="pt-align-right"><span className="pt-icon-standard pt-icon-cross" onClick={()=>this.props.dispatch.app.removeError(idx)}/></div></div>
                        <div><p>{error.title}</p><p>{error.body}</p></div></div>)}
            </div>
        );
    }
}

export default map(ErrorList);