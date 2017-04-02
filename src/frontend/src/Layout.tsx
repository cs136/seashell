// import React from "react";
import * as React from "react";
import {Route, RouteComponentProps} from "react-router";
import Navigation from "./partials/Navigation";
import {map, actionsInterface} from "./actions";
import Project from "./views/Project";

import Home from "./views/Home";

const styles = require("./Layout.scss");

require("@blueprintjs/core/dist/blueprint.css");

export interface LayoutProps { };
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
          <Route exact path="/" component={Home} />
          <Route path="/project/:id" component={Project} />
        </div>
      </div>
    );
  }
}

export default map<LayoutProps>(Layout);
