import * as React from "react";
import { BrowserRouter } from "react-router-dom";
import { Router, Route } from "react-router";
import Layout from "./Layout";
import SignIn from "./views/SignIn";
import {map, actionsInterface} from "./actions";

export interface AppProps { title: string; }
export interface AppState { open?: boolean; title?: string; }

class App extends React.Component<AppProps&actionsInterface, AppState> {
  render() {
    const projects = this.props.appState.projects;
    return this.props.user.questid == null ? <SignIn/> :
    (<BrowserRouter>
      <Layout/>
    </BrowserRouter>);
  }
}

export default map(App);
