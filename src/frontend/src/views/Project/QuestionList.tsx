import * as React from "react";
import {Menu, MenuItem} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";

const styles = require("./Project.scss");

interface QuestionListProps {  }

interface QuestionListState {  }

class QuestionList extends React.Component<QuestionListProps & actionsInterface, QuestionListState> {

    constructor(props: QuestionListProps & actionsInterface) {
      super(props);
    }

    render() {
      const project = this.props.appState.currentProject;
      console.log("switchquestion-diag");
      if (project)
        return (<Menu>
          {project.questions.map((question: string) =>
            (<MenuItem onClick={() =>
              this.props.dispatch.question.switchQuestion(project.id, question).then((question) => {
                if (question.openFiles.length > 0) {
                  this.props.dispatch.file.switchFile(project.id, question.openFiles[0]);
                }
              })} key={"question-list-item-" + question} iconName="comment" text={question} />))}
          <MenuItem onClick={() => this.props.dispatch.dialog.toggleAddQuestion()}
            iconName="add" text="Add Question" />
        </Menu>);
      else
        throw new Error("Invoking QuestionList on undefined project!");
    }
}

export default map<QuestionListProps>(QuestionList);
