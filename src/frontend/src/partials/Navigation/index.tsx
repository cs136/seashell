import * as React from "react";
import {map, actionsInterface} from "../../actions";
import {Link} from "react-router";
import {Menu, MenuItem, Dialog, Popover, Position} from "@blueprintjs/core";
import {HelpDialog} from "./Dialogs";
import SettingsDialog from "./Dialogs";
import * as R from "ramda";
import ErrorList from "./Errors";


const logo = require<any>("./logo.svg");
const styles = require<any>("./index.scss");
const layoutStyles = require<any>("../../Layout.scss");


export interface NavigationProps { navLeft?: JSX.Element[]; navRight?: JSX.Element[]; className?: string; }
export interface NavigationState { helpVisible?: boolean; settingsVisible?: boolean; }

class Navigation extends React.Component<NavigationProps&actionsInterface, NavigationState> {
  constructor(props: NavigationProps&actionsInterface) {
    super(props);
    this.state = {
      helpVisible: false,
      settingsVisible: false,
    };
  }
  toggleHelp() {
    this.setState({helpVisible: !this.state.helpVisible});
  }
  toggleSettings() {
    this.setState({settingsVisible: !this.state.settingsVisible});
  }
  render() {
    return (<div>
      <ErrorList />
      <nav className={"pt-navbar " + styles.navbar}>
        <div className="pt-navbar-group pt-align-left">
          <Link to="/" className={"pt-button pt-minimal " + styles.home}><img src={logo} className={styles.logo} alt="Seashell"/></Link>
          {this.props.navLeft}
        </div>
        <div className="pt-navbar-group pt-align-right">
          {this.props.navRight}
          <div><Popover content={<Menu>
            <MenuItem iconName="help" text="Help" onClick={this.toggleHelp.bind(this)}/>
            <MenuItem iconName="refresh" text="Sync All" />
            <MenuItem iconName="cog" text="Settings" onClick={this.toggleSettings.bind(this)} />
            <MenuItem iconName="changes" text="Reset Seashell" />
            <MenuItem iconName="log-out" text="Sign Out" />
        </Menu>} position={Position.BOTTOM_RIGHT}>
            <button className="pt-button pt-icon-more pt-minimal"></button>
        </Popover>
            <Dialog className={styles.dialogStyle} title="Seashell Help" isOpen={this.state.helpVisible} onClose={this.toggleHelp.bind(this)}>
              <HelpDialog />
            </Dialog>
            <Dialog className={styles.dialogStyle} title="Settings" isOpen={this.state.settingsVisible} onClose={this.toggleSettings.bind(this)}>
              <SettingsDialog closefunc={this.toggleSettings.bind(this)}/>
            </Dialog></div>
        </div>
      </nav></div>
    );
  }
}
export default map<NavigationProps>(Navigation);
