import * as React from "react";
import {map, actionsInterface} from "../../actions";
import {Link} from "react-router-dom";
import {Menu, MenuItem, Dialog, Popover, Position} from "@blueprintjs/core";
import {HelpDialog} from "./Dialogs";
import SettingsDialog from "./Dialogs";
import * as R from "ramda";
import AddProjectWindow from "./AddProject";
import ResetWindow from "./Reset";

const logo = require("../../assets/logo.svg");
const styles = require("./index.scss");
const layoutStyles = require("../../Layout.scss");


export interface NavigationProps { navLeft?: JSX.Element[]; navRight?: JSX.Element[]; className?: string; }
export interface NavigationState {  }

class Navigation extends React.Component<NavigationProps&actionsInterface, NavigationState> {
  constructor(props: NavigationProps&actionsInterface) {
    super(props);
    this.state = {
      helpVisible: false,
      settingsVisible: false,
    };
  }
  render() {
    return (
      <nav className={"pt-navbar " + styles.navbar}>
        <div className="pt-navbar-group pt-align-left">
          <Link to="/" className={"pt-button pt-minimal " + styles.home}>
            <img src={logo} className={styles.logo} alt="Seashell"/>
          </Link>
          {this.props.navLeft}
        </div>
        <div className="pt-navbar-group pt-align-right">
          {this.props.navRight}
          <Popover
            className={styles.options}
            content={
              <Menu>
                  <MenuItem iconName="help" text="Help" onClick={this.props.dispatch.dialog.toggleHelp}/>
                  <MenuItem iconName="cog" text="Settings" onClick={this.props.dispatch.dialog.toggleSettings} />
                  <MenuItem iconName="changes" text="Reset Seashell" onClick={() => {
                    this.props.dispatch.dialog.toggleResetOpen(); }}/>
                  <MenuItem iconName="log-out" text="Sign Out" onClick={this.props.dispatch.user.signout}/>
              </Menu>
            }
            position={Position.BOTTOM_RIGHT}>
            <button className="pt-button pt-icon-more pt-minimal"></button>
        </Popover>
          <div>
            <Dialog className={styles.dialogStyle} title="Seashell Help"
              isOpen={this.props.dialog.help_open}
              onClose={this.props.dispatch.dialog.toggleHelp}>
              <HelpDialog />
            </Dialog>
            <Dialog className={styles.dialogStyle} title="Settings"
              isOpen={this.props.dialog.settings_open}
              onClose={this.props.dispatch.dialog.toggleSettings}>
              <SettingsDialog closefunc={this.props.dispatch.dialog.toggleSettings}/>
            </Dialog>
            <Dialog className={styles.dialogStyle}
              title="Add Project"
              isOpen={this.props.dialog.add_project_open}
              onClose={this.props.dispatch.dialog.toggleAddProject}>
              <AddProjectWindow closefunc={this.props.dispatch.dialog.toggleAddProject}/>
            </Dialog>
            <Dialog isCloseButtonShown={false} className={styles.dialogStyle} title="Reset Seashell / Log in again"
              isOpen={this.props.dialog.reset_open}
              onClose={() => {
                if (! this.props.user.busy) {
                  this.props.dispatch.dialog.toggleResetOpen();
                }
              }}>
              <ResetWindow closefunc={this.props.dispatch.dialog.toggleResetOpen} />
            </Dialog>
          </div>
        </div>
      </nav>
    );
  }
}
export default map<NavigationProps>(Navigation);
