import * as React from 'react';
import * as Terminal from 'xterm';

export interface ConsoleProps {style: any};
export interface ConsoleState {};


export default class Xterm extends React.Component <ConsoleProps, ConsoleState> {
    term: any;
    constructor(props: ConsoleProps, context: any){
        super(props, context);
        this.term = new Terminal({
            cursorBlink: true
        });
        this.term.open(document.getElementById("console"));
    }

    componentDidMount() {


        //term.setOption('cursorStyle', 'underline');


        this.term.prompt = () => {
            this.term.write("\r\n > ");
        };

        this.term.on('key', (key: Number, evt: any) => {
            let printable = (!evt.altKey && !evt.altGraphKey && !evt.ctrlKey && !evt.metaKey && evt.keyCode != '38' && evt.keyCode != '40' && evt.keyCode != '39' && evt.keyCode != '37');
            if (evt.keyCode == '13') {
                this.term.prompt();
            } else if (evt.keyCode == '8') {
                if (this.term.x > 3)
                    this.term.write("\b \b");
            }
            else if (printable) this.term.write(key);
        });
        this.term.resize(30, 50);
        this.term.prompt();
        this.term.focus();
    }
    componentWillUnmount(){
        this.term.destroy();
    }

    render() {
        return(<div className="console" style={this.props.style}></div>);
    }
}     
