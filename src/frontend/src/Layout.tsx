// import React from "react";
import * as React from "react";
import {withRouter, Switch, Route, RouteComponentProps} from "react-router";
import Navigation from "./partials/Navigation";
import {map, actionsInterface} from "./actions";
import Project from "./views/Project";

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
    const question = this.props.appState.currentProject.currentQuestion;
    return (
      <div className={styles.app + " " + (this.props.settings.theme === 0 ? "pt-dark" : "")}>
        <div className={styles.gradientBar} />
        <div className={styles.mainContent}>
          <Switch>
            <Route exact path="/" component={Home} />
            <Route path="/project/:id" component={Project} />
          </Switch>
        </div>
      </div>
    );
  }
}

export default withRouter(map<LayoutProps>(Layout));
