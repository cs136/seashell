import * as React from "react";
import * as Terminal from "xterm";
import { merge } from "ramda";
import { Services } from "../../helpers/Services";

Terminal.loadAddon("fit");

export interface ConsoleProps { readOnly: boolean; style?: any; className?: string; dispatch: any; };
export interface ConsoleState { input: boolean; line: number; currString: string; };

const lightStyles = require("./xterm.scss");
const styles = require("xterm/lib/xterm.css");

export default class Xterm extends React.Component<ConsoleProps, ConsoleState> {
    term: any;
    container?: HTMLElement;

    constructor(props: ConsoleProps, context: any) {
        super(props, context);
        this.term = new Terminal({
            cursorBlink: true
        });
        this.container = null;
    }

    dataReceived(payload: string) {
        this.term.write(payload.replace(/\r?\n/g, "\r\n"));
    }

    clear(): void {
        this.term.clear();
    }

    getLines(): number {
        return document.getElementsByClassName("xterm-rows")[0].childElementCount;
    }

    setHeight(height: Number) {
        this.container.style.height = height + "px";
    }

    setFlex(flex: any) {
        this.container.style.flex = flex;
    }

    updateLayout() {
        this.term.fit();
    }

    componentWillMount(){
        this.props.dispatch.app.setTerm(this.dataReceived.bind(this), this.clear.bind(this));
    }

    componentDidMount() {
        this.term.open(this.container);
        this.setState({ input: true, line: 1, currString: "" });
        const consoleElement: HTMLElement = this.container;
        this.term.on("key", (key: string, evt: any) => {
            if (!this.props.readOnly) {
                Services.compiler().programInput(key);
            }
        });
    }

    componentWillUnmount() {
        this.props.dispatch.app.setTerm(null, null);
        this.term.destroy();
    }

    render() {
        let style = {...this.props.style};
        return(<div style={this.props.style} className={this.props.className} ref={
            (container: HTMLElement) => { this.container = container; }}></div>);
    }
}
