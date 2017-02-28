import * as React from 'react';
import * as Xterm from 'xterm';

export interface ConsoleProps {};
export interface ConsoleState {};

let term = new Xterm({
    cursorBlink: true
});

term.open(document.getElementById("app"));

class Terminal extends React.Component <ConsoleProps, ConsoleState> {

    componentDidMount() {
        const container = document.getElementById("app");

        //term.setOption('cursorStyle', 'underline');

        term.open(container);

        term.prompt = () => {
            term.write("\r\n > ");
        };

        term.on('key', (key: Number, evt: any) => {
            let printable = (!evt.altKey && !evt.altGraphKey && !evt.ctrlKey && !evt.metaKey && evt.keyCode != '38' && evt.keyCode != '40' && evt.keyCode != '39' && evt.keyCode != '37');
            if (evt.keyCode == '13') {
                term.prompt();
            } else if (evt.keyCode == '8') {
                if (term.x > 3)
                    term.write("\b \b");
            }
            else if (printable) term.write(key);
        });

        term.prompt();
        term.focus();
    }

    render() {
        return(<div className="console"></div>);
    }
}     