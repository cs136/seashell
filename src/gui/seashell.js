var seashellEditor = document.getElementById('seashell');
//var txt = document.createTextNode("woohoo");
//seashellEditor.appendChild(txt);
seashellEditor.innerHTML = ['#include &lt;stdio.h&gt;',
'int main() {',
'    int i;',
'    for (i = 0; i &lt;3; i++) {',
'        printf("She sells C shells by the sea shore");',
'    }',
'    return(0);',
'}'].join('\n');

var editor = CodeMirror.fromTextArea(seashellEditor, {lineNumbers: true});
var welcomeMessage = 'Welcome to Seashell! Messages and program output will appear here.';
var console = CodeMirror(document.getElementById('console'), 
                            {value: welcomeMessage, 
                            readOnly: true, 
                            theme: 'dark-on-light'});
var compiled = false;
editor.on("change", mark_changed);

function mark_changed(instance, chobj) {
    compiled = false;
}

function console_write(str) {
    console.setOption('readOnly', false);
    var newText = console.getValue() + '\n' + str;
    console.setValue(newText);
    console.setOption('readOnly', true);
}

/** handlers for buttons that only affect the client-side **/
function toggleCommentHandler(isC) {
    var from = editor.getCursor(true);
    var to = editor.getCursor(false);
    editor.commentRange(isC, from, to);
}
function autoIndentHandler() {
    var from = editor.getCursor(true);
    var to = editor.getCursor(false);
    editor.autoFormatRange(from, to);
}

function gotoHandler() {
    var gotoPrompt = 'Line: <input type="text" style="width: 3em"/>';
    editor.openDialog(gotoPrompt, function(query) {
            editor.setCursor(query-1, 0); });
}

/** handlers for buttons that need to interact with the back-end **/

function submitHandler() {
    var submitPrompt = 'Assignment ID: <input type="text" style="width: 3em"/>';
    editor.openDialog(submitPrompt, function(query) {
            editor.setCursor(query-1, 0); });
}

function compileHandler() {
    if (!compiled) {
        // TODO save file
        // TODO compile file
        compiled = true;
        console_write('Done compiling.');
    } else {
        console_write('Already compiled.');
    }
}

function runHandler() {
    if (!compiled) {
        console_write('The source file was modified since the last compile. Compiling first...');
        compileHandler();
    }
    // TODO run
}

function runInputHandler() {
    var filePrompt = 'Name of input file: <input type="text" style="width: 3em"/>';
    editor.openDialog(filePrompt, 
                        function(query) {
                            // TODO run
                        });
}

function saveHandler() {
    var filePrompt = 'Save as: <input type="text" style="width: 3em"/>';
    editor.openDialog(filePrompt, 
                        function(query) {
                            console_write('Your file has been saved as ' + query + '.');
                        });
}

function openFileHandler() {
    var filePrompt = 'File name: <input type="text" style="width: 3em"/>';
    editor.openDialog(filePrompt, 
                        function(query) {
// TODO
//                            if (successful) {
                                console_write('Opened file ' + query + '.');
//                            else {
//                                console_write('Failed to open the file ' + query + '.');
//                            }
                        });
}

function newFileHandler() {
    var filePrompt = 'Name of new file: <input type="text" style="width: 3em"/>';
    editor.openDialog(filePrompt, 
                        function(query) {
// TODO
//                            if (successful) {
                                console_write('Creating file ' + query + '.');
//                            else {
//                                console_write('Failed to create the file ' + query + '.');
//                            }
                        });
}

/** attach actions to all the buttons. **/
function giveAction(elid,act) {
    document.getElementById(elid).onclick=act;
}

giveAction("undo", function() {editor.undo();});
giveAction("redo", function() {editor.redo();});
        
giveAction("comment", function() {toggleCommentHandler(true);});
giveAction("uncomment", function() {toggleCommentHandler(false);});
giveAction("autoindent", autoIndentHandler);
giveAction("goto-line", gotoHandler);
giveAction("submit-assignment", submitHandler);

giveAction("clear-console", function() {console.setValue('')});
giveAction("compile", compileHandler);
giveAction("run", runHandler);
giveAction("run-input", runInputHandler);
giveAction("save-file", saveHandler);
giveAction("open-file", openFileHandler);
giveAction("new-file", newFileHandler);

/*
var termConf= {
x: 0,
y: 0,
frameWidth: 0,
crsrBlinkMode: true,
termDiv: 'termPrompt',
closeOnESC: false,
handler: termHandler,
ps: '>',
greeting: 'Messages and program output will appear here.',
cols: 93,
rows: 10
}

function termHandler() {
        //this.cursorOn();
        var line = this.lineBuffer;
        this.newLine();
        if (line == "help") {
          this.write(helpPage)
        }
        else if (line == "exit") {
          this.close();
          return;
        }
        else if (line != "") {
          this.write("You typed: "+line);
        }
        this.prompt();
}

var term = new Terminal(termConf);
function enable_console() { TermGlobals.keylock = false; term.cursorOn(); }
function disable_console() { TermGlobals.keylock = true; term.cursorOff();}

term.open();
disable_console();
document.getElementById('bottom').onmouseover=enable_console();
document.getElementById('bottom').onmouseout=disable_console();

*/

//console_write("foo");
//console_write("bar");
editor.focus();
