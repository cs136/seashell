import * as React from "react";

import { merge } from "ramda";

import * as Blueprint from "@blueprintjs/core";
import { map, actionsInterface } from "../../actions";
import { Services } from "../../helpers/Services";

export interface ResetWindowProps { closefunc: Function; };

class ResetWindow extends React.Component<ResetWindowProps & actionsInterface, {}> {
    constructor(props: ResetWindowProps & actionsInterface) {
        super(props);
    };
    render() {
        return (
        <div className="pt-dialog-body">
            <form className="pt-vertical"
                  onSubmit={(e) => {
                        e.preventDefault();
                        this.props.dispatch.user.signin((this.refs.username as HTMLInputElement).value,
                                                        (this.refs.password as HTMLInputElement).value,
                                                        (this.refs.reset as HTMLInputElement).checked).then(() => {
                            if ((this.refs.reset as HTMLInputElement).checked) {
                                window.location.reload(true);
                            } else {
                                this.props.closefunc();
                            }
                        });
                    }}>
                <label className="pt-label">
                    Please type in your credentials to log in again. Please make sure "reset" is checked if you also want to reset Seashell.
                </label>
                <div className="pt-form-group">
                    <div className="pt-input-group pt-large">
                        <span className="pt-icon pt-icon-person"></span>
                        <input className="pt-input pt-fill"
                            type="text"
                            disabled={this.props.user.busy}
                            ref="username"
                            defaultValue={Services.session().username}
                            placeholder="Username"/>
                    </div>
                    <div className="pt-input-group pt-large">
                        <span className="pt-icon pt-icon-lock"></span>
                        <input className="pt-input pt-fill"
                                type="password"
                                ref="password"
                                disabled={this.props.user.busy}
                                placeholder="Password"/>
                    </div>
                </div>
                <div className="pt-control-group pt-large">
                    <label className="pt-control pt-checkbox">
                        <input type="checkbox" ref="reset" defaultChecked />
                        <span className="pt-control-indicator"></span>
                        Reset
                    </label>
                </div>
                <div className="pt-control-group">
                    <button type="button"
                            className="pt-button"
                            disabled={this.props.user.busy}
                            onClick={() => {
                                this.props.closefunc();
                            }}>
                        Cancel
                    </button>
                    <button type="submit"
                            className="pt-button pt-intent-primary"
                            disabled={this.props.user.busy}>
                        Log in again
                    </button>
                </div>
            </form>
        </div>);
    }
}

export default map<ResetWindowProps>(ResetWindow);
