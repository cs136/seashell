import * as React from "react";
import { Router, Route, IndexRoute, hashHistory } from "react-router";
import Layout from "./Layout";
import Project from "./views/Project";
import Home from "./views/Home";


export default () => (
  <Router history={hashHistory} >
    <Route path="/" component={Layout}>
      <IndexRoute component={Home} />
      <Route path="project/:id" component={Project} />
    </Route>
  </Router>
);
