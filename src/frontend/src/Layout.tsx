// import React from "react";
import * as React from "react";
import Navigation from "./partials/Navigation";
import {map, actionsInterface} from "./actions";

const styles = require("./Layout.scss");


require("@blueprintjs/core/dist/blueprint.css");

export interface LayoutProps { title: string; }
export interface LayoutState { open?: boolean; title?: string; }

class Layout extends React.Component<LayoutProps & actionsInterface, LayoutState> {
  constructor(props: LayoutProps & actionsInterface) {
    super(props);
    this.state = {
      open: false,
      title: props.title,
    };
    this.handleToggle = this.handleToggle.bind(this);
  }

  handleToggle() {
    this.setState({ open: !this.state.open });
  }

  render() {
    return (
      <div className={styles.app + " " + (this.props.settings.theme === 0 ? "pt-dark" : "")}>
        <div className={styles.gradientBar} />
        <div className={styles.mainContent}>{this.props.children}</div>
      </div>
    );
  }
}

export default map<LayoutProps>(Layout);
