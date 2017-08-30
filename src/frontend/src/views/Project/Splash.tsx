import * as React from "react";
import {Menu, MenuItem} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import QuestionList from "./QuestionList";
import FileList from "./Files/List";

const styles = require("./Project.scss");

export interface SplashProps { };
export interface SplashState {  }

class Splash extends React.Component<SplashProps & actionsInterface, SplashState> {
    constructor(props: SplashProps & actionsInterface) {
        super(props);
    }
    render() {
        const project = this.props.appState.currentProject;
        if (! project) {
            return <div></div>;
        }
        const question = project.currentQuestion;
        return (<div className={styles.splashContainer}>
            <h4>{project.name + (question ? "/" + question.name : "")}</h4>
            <p>Select a {question ? "file" : "question" } to get started</p>
            {question ? <FileList /> : <QuestionList />}
        </div>);
    }
}
export default map<SplashProps>(Splash);
