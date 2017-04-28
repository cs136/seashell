import * as React from "react";
import {map, actionsInterface} from "../actions";
const layoutStyles = require("../Layout.scss");
const styles = require("./SignIn.scss");

const logo = require("../assets/logo.svg");

export interface SignInProps { }
export interface SignInState { }

class SignIn extends React.Component<SignInProps&actionsInterface, SignInState> {
    constructor(props: SignInProps&actionsInterface) {
        super(props);
        this.signin = this.signin.bind(this);
    }
    signin(e: any) {
        e.preventDefault();
        this.setState({loading: true});
        const ctx = this;
        this.props.dispatch.user.signin((this.refs.username as HTMLInputElement).value, (this.refs.password as HTMLInputElement).value, false).then(() => {
            ctx.props.dispatch.project.getAllProjects();
            ctx.props.dispatch.settings.initSettings();
        }).catch((reason) => {
            if (reason !== null) throw reason;
        });
    }
    render() {
        const projects = this.props.appState.projects;
        return (<div className={styles.container}>
                <img src={logo} className={styles.logo}/><h5>Sign in to Seashell</h5>
                <form className="pt-control-group pt-vertical" onSubmit={this.signin}>
                    <div className="pt-input-group pt-large">
                        <span className="pt-icon pt-icon-person"></span>
                        <input type="text" className="pt-input"
                            disabled={this.props.user.busy}
                            ref="username" placeholder="Username" />
                    </div>
                    <div className="pt-input-group pt-large">
                        <span className="pt-icon pt-icon-lock"></span>
                        <input type="password" className="pt-input"
                            disabled={this.props.user.busy} ref="password" placeholder="Password" />
                    </div>
                    <button className="pt-button pt-large pt-intent-primary"
                        disabled={this.props.user.busy}>Sign In</button>
                </form>

            </div>);
    }
}

export default map(SignIn);
