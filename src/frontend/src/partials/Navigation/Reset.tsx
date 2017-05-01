import * as React from "react";

import { merge } from "ramda";

import * as Blueprint from "@blueprintjs/core";
import { map, actionsInterface } from "../../actions";

export interface ResetWindowProps { closefunc: Function; reset: boolean; };

class ResetWindow extends React.Component<ResetWindowProps & actionsInterface, { username: string, password: string, reset: boolean }> {
    constructor(props: ResetWindowProps & actionsInterface) {
        super(props);
        this.state = {
            username: "",
            password: "",
            reset: this.props.reset,
        };
    }
    render() {
        return (<div className="pt-dialog-body">
            <p>Please type in your credentials to log in again. Please make sure "Reset Seashell" is checked if you also want to reset Seashell.</p>
            <div>
                <div>Username:</div>
                <div>
                    <input className="pt-input pt-fill" required type="text" value={this.state.username}
                        onChange={(e => this.setState(merge(this.state, { username: e.currentTarget.value })))} /></div></div>
            <div>
                <div>Password:</div>
                <div>
                    <input className="pt-input pt-fill" required type="password" value={this.state.password}
                        onChange={(e => this.setState(merge(this.state, { password: e.currentTarget.value })))} />
                </div>
            </div>
            <p/>
            <div>
                <Blueprint.Checkbox checked={this.state.reset} label="Reset Seashell" onChange={() => this.setState(merge(this.state, {reset: !this.state.reset}))}/>
            </div>
            <div className="pt-button-group">
                <button type="button" className="pt-button" onClick={() => {
                    this.props.closefunc();
                }}>Cancel</button>
                <button type="button" className="pt-button pt-intent-primary" disabled={this.state.username === "" || this.state.password === ""} onClick={() => {
                    this.props.dispatch.user.signin(this.state.username, this.state.password, this.state.reset).then(() => {
                        this.props.dispatch.project.getAllProjects();
                        this.props.dispatch.settings.initSettings();
                        this.props.closefunc();
                    });
                }}>Log in again</button>
            </div>
        </div>
        );
    }
}

export default map<ResetWindowProps>(ResetWindow);
