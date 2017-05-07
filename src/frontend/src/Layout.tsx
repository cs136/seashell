// import React from "react";
import * as React from "react";
import {withRouter, Switch, Route, RouteComponentProps} from "react-router";
import Navigation from "./partials/Navigation";
import {map, actionsInterface} from "./actions";
import Project from "./views/Project";
import {Tooltip} from "@blueprintjs/core";

import Home from "./views/Home";

const styles = require("./Layout.scss");

require("@blueprintjs/core/dist/blueprint.css");

export interface LayoutProps extends RouteComponentProps<{}> { };
export interface LayoutState { };

class Layout extends React.Component<LayoutProps & actionsInterface, LayoutState> {
  constructor(props: LayoutProps & actionsInterface) {
    super(props);
  }

  render() {
    return (
      <div className={styles.app + " " + (this.props.settings.theme === 0 ? "pt-dark" : "")}>
        {this.props.appState.connected ?
          <div className={styles.gradientBar} /> :
          <div className={styles.disconnectedGradientBar}>
            The websocket is currently disconnected.
          </div>}
        <div className={styles.mainContent}>
          <Route exact path="/" component={Home} />
          <Route path="/project/:id/:name" component={Project} />
        </div>
      </div>
    );
  }
}

export default map<{}>(withRouter(Layout));
