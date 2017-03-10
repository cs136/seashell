import * as React from "react";
import {map, actionsInterface} from "../../actions";
import {Link} from "react-router";
import {Tooltip, Position, Dialog} from "@blueprintjs/core";
import {HelpDialog} from "./Dialogs";
import SettingsDialog from "./Dialogs";
import * as R from "ramda";


const logo = require<any>("./logo.svg");
const styles = require<any>("./index.scss");
const layoutStyles = require<any>("../../Layout.scss");


export interface NavigationProps { title: string; }
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
      <nav className={styles.navbar} style={{height: window.innerHeight}}>
        <Link to="/" className={"pt-button pt-large pt-minimal " + styles.home}><img src={logo} className={styles.logo} alt="Seashell"/></Link>
        <div className={styles.navbarBottom}>
          <Tooltip content="Help" position={Position.RIGHT}><button className="pt-button pt-minimal pt-large pt-icon-help" onClick={this.toggleHelp.bind(this)}></button></Tooltip>
          <Tooltip content="Sync All" position={Position.RIGHT}><button className="pt-button pt-minimal pt-large pt-icon-import"></button></Tooltip>
          <Tooltip content="Settings" position={Position.RIGHT}><button className="pt-button pt-minimal pt-large pt-icon-wrench" onClick={this.toggleSettings.bind(this)}></button></Tooltip>
          <Tooltip content="Log Out" position={Position.RIGHT}><button className="pt-button pt-minimal pt-large pt-icon-log-out"></button></Tooltip>
        </div>
      </nav>
      <Dialog className={styles.dialogStyle} title="Seashell Help" isOpen={this.state.helpVisible} onClose={this.toggleHelp.bind(this)}>
        <HelpDialog />
      </Dialog>
      <Dialog className={styles.dialogStyle} title="Settings" isOpen={this.state.settingsVisible} onClose={this.toggleSettings.bind(this)}>
        <SettingsDialog closefunc={this.toggleSettings.bind(this)}/>
      </Dialog>
    </div>);
  }
}
export default map(Navigation);
