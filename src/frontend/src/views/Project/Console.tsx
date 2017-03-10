import * as React from "react";
import * as Terminal from "xterm";
import {merge} from "ramda";

Terminal.loadAddon("fit");

export interface ConsoleProps {};
export interface ConsoleState {input: boolean; line: number; currString: string; };

const styles = require<any>("./xterm.css");

export default class Xterm extends React.Component <ConsoleProps, ConsoleState> {
    term: any;
    constructor(props: ConsoleProps, context: any) {
        super(props, context);
        this.term = new Terminal({
            cursorBlink: true
        });
    }

    dataReceived(payload: string) {
        this.term.eraseLine(this.term.y);
        this.term.x = 0;
        this.term.y = this.state.line;
        this.term.write(" " + payload);
        this.term.prompt();
        this.term.refresh(this.state.line, this.state.line + 1, false);
        this.term.write(this.state.currString);
        this.setState(merge(this.state, {line: this.term.y + 1}));
    }

    setHeight(height: Number) {
        (this.refs.console as HTMLDivElement).style.height = height + "px";
    }

    updateLayout() {
        this.term.fit();
    }

    componentDidMount() {
        this.term.open(this.refs.console);
        this.setState({input: true, line: 1, currString: ""});
        this.term.prompt = () => {
            this.term.write("\r\n > ");
        };
        const consoleElement: HTMLElement = (this.refs.console as HTMLElement);
        this.term.on("key", (key: string, evt: any) => {
            if (evt.which === 47) {
                this.dataReceived("Data Received");
                return;
            }
            if (! this.state.input) {
                this.term.cursorBlink = false;
                return;
            }
            let printable = (!evt.altKey && !evt.altGraphKey && !evt.ctrlKey && !evt.metaKey);
            if (evt.which === 13) {
                this.setState(merge(this.state, {line: this.term.y + 1, currString: ""}));
                this.term.prompt();
            } else if (evt.which === 8) {
                if (this.term.x > 3 || this.term.y > this.state.line) {
                    if (this.term.x === 0) {
                        this.term.x = consoleElement.getBoundingClientRect().width;
                        this.term.y -= 1;
                        this.term.refresh(this.term.y, this.term.y + 1, false);
                    }
                    this.term.write("\b \b");
                    this.setState(merge(this.state, {currString: this.state.currString.slice(0, -1)}));
                }
            }
            else if (printable) {
                this.term.write(key);
                this.setState(merge(this.state, {currString: this.state.currString + key}));
            }
        });
        this.term.prompt();
    }

    componentWillUnmount() {
        this.term.destroy();
    }

    render() {
        return(<div style={{background: "#000"}} ref="console"></div>);
    }
}
