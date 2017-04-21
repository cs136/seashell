import * as React from "react";
import {Menu, MenuItem} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";

const styles = require("./Project.scss");

export interface QuestionListProps { project: any; };
export interface QuestionListState {  }

class QuestionList extends React.Component<QuestionListProps & actionsInterface, QuestionListState> {
    constructor(props: QuestionListProps & actionsInterface) {
        super(props);
    }
    render() {
        const project = this.props.project;
        return (<Menu>
            {project.questions.map((question: string) => (<MenuItem onClick={() => this.props.dispatch.question.switchQuestion(this.props.appState.currentProject.name, question)} key={"question-list-item-" + question} iconName="comment" text={question} />))}
        </Menu>);
    }
}
export default map<QuestionListProps>(QuestionList);
