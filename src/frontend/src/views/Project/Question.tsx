import * as React from "react";
import {Position, Popover, Button} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import File from "./File";
import Loading from "./Loading";

const styles = require<any>("./project.scss");

export interface QuestionProps { question: string; };
export interface QuestionState {  }

class Question extends React.Component<QuestionProps & actionsInterface, QuestionState> {
    constructor(props: QuestionProps & actionsInterface) {
        super(props);
    }
    render() {
        const question = this.props.appState.currentProject.currentQuestion;

        return (this.props.question === question.name ? (<div>
            <nav className={styles.actionbar + " pt-navbar"}>
                <div className="pt-navbar-group pt-align-left">
                    <button className="pt-button pt-minimal">Move/Rename</button>
                    <button className="pt-button pt-minimal">Copy</button>
                    <button className="pt-button pt-minimal">Delete</button>
                    <button className="pt-button pt-minimal">Set as <span className="pt-icon-standard pt-icon-play" />File</button>
                </div>
                <div className="pt-navbar-group pt-align-right">
                    <button className="pt-button pt-minimal">Test</button>
                    <button className="pt-button pt-minimal"><span className="pt-icon-standard pt-icon-play" />Run</button>
                </div>
            </nav>
            <File file={question.currentFile.name} /></div>
        ) : <Loading />);
    }
}
export default map<QuestionProps>(Question);