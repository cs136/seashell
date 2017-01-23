// import React from 'react';
import * as React from 'react';
import Navigation from './partials/Navigation';

const styles = require<any>('./Layout.scss');


require<any>('../node_modules/@blueprintjs/core/dist/blueprint.css');

export interface LayoutProps { title: string; }
export interface LayoutState { open?: boolean; title?: string; }

export default class Layout extends React.Component<LayoutProps, LayoutState> {
  constructor(props:LayoutProps) {
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
      <div className={styles.app}>
        <Navigation />
        <div className={styles.mainContent}>{this.props.children}</div>
      </div>
    );
  }
}

