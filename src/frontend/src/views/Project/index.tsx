import * as React from "react";
import {Tab2, Tabs2} from "@blueprintjs/core";
import {map, actionsInterface} from "../../actions";
import Question from "./Question";

const styles = require<any>("./project.scss");
const layoutStyles = require<any>("../../Layout.scss");


export interface ProjectProps { title: string; routeParams: any; }
export interface ProjectState { open?: boolean; title?: string; editor?: any; terminal?: any; }



class Project extends React.Component<ProjectProps&actionsInterface, ProjectState> {
  constructor(props: ProjectProps&actionsInterface) {
    super(props);
    this.state = {editor: null};
  }
  render() {
    const project = this.props.appState.currentProject;
    return (<Tabs2 renderActiveTabPanelOnly={true} id={"project-" + project} className={styles.questionTabList}>
        <h5 className={styles.questionName}>{project.name}</h5>
        {project.questions.map((question) => (<Tab2 className={styles.tabPanel} id={"question-tab-" + question} key={"question-tab-" + question} title={question} panel={<Question question={question} />}/>))}
      </Tabs2>);
  }
}

export default map<ProjectProps>(Project);
