import * as React from "react";
import {Tab2, Tabs2} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import MonacoEditor from "react-monaco-editor";
import Xterm from "./Console";
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
        const project = this.props.appState.currentProject;
        return (this.props.question === project.currentQuestion.name ? (<Tabs2 className={styles.fileTabList} id={"question-" + project}>
            {project.currentQuestion.files.map((file) => (<Tab2 id={"file-tab-" + file} className={styles.tabPanel} key={"file-tab-" + file} title={file} panel={<File file={file} />}/>))}
        </Tabs2>) : <Loading />);
    }
}
export default map<QuestionProps>(Question);