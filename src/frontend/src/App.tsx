import * as React from "react";
import { Router, Route, IndexRoute, hashHistory } from "react-router";
import Layout from "./Layout";
import Project from "./views/Project";
import SignIn from "./views/SignIn";
import Home from "./views/Home";
import {map, actionsInterface} from "./actions";

export interface AppProps { title: string; }
export interface AppState { open?: boolean; title?: string; }

class App extends React.Component<AppProps&actionsInterface, AppState> {
  render() {
    const projects = this.props.appState.projects;
    return this.props.user.questid == null ? <SignIn/> : (
    <Router history={hashHistory} >
      <Route path="/" component={Layout}>
        <IndexRoute component={Home} />
        <Route path="project/:id" component={Project} />
      </Route>
    </Router>);
  }
}

export default map(App);
