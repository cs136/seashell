var compiled = false;

function giveAction(elid,act) {
    document.getElementById(elid).onclick=act;
}
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
var editor = CodeMirror.fromTextArea(seashellEditor, {lineNumbers: true, onKeyEvent: ssKeyHandler});

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

function submitHandler() {
    var submitPrompt = 'Assignment ID: <input type="text" style="width: 3em"/>';
    editor.openDialog(submitPrompt, function(query) {
            editor.setCursor(query-1, 0); });
}

function compileHandler() {
    if (!compiled) {
        compiled = true;
        // TODO
        term.write('\nDone compiling.\n');
        term.prompt();
    } else {
        term.write('\nAlready compiled.\n');
        term.prompt();
    }
}
function runHandler() {
    if (!compiled) {
        term.write('\nThe source file was modified since the last compile. Compiling first...\n');
        compileHandler();
    }
    // run
}

function runInputHandler() {
    var filePrompt = 'Name of input file: <input type="text" style="width: 3em"/>';
    editor.openDialog(filePrompt, function(query) {
            editor.setCursor(query-1, 0); });
}

function saveHandler() {
    var filePrompt = 'Save as: <input type="text" style="width: 3em"/>';
    editor.openDialog(filePrompt, function(query) {
            editor.setCursor(query-1, 0); });
    term.write('\nYour file has been saved.***STUB***\n');
    term.prompt();
}

function openFileHandler() {
    var filePrompt = 'File name: <input type="text" style="width: 3em"/>';
    editor.openDialog(filePrompt, function(query) {
            editor.setCursor(query-1, 0); });
}

function newFileHandler() {
    var filePrompt = 'Name of new file: <input type="text" style="width: 3em"/>';
    editor.openDialog(filePrompt, function(query) {
            editor.setCursor(query-1, 0); });
}

giveAction("undo", function() {editor.undo();});
giveAction("redo", function() {editor.redo();});
        
giveAction("comment", function() {toggleCommentHandler(true);});
giveAction("uncomment", function() {toggleCommentHandler(false);});
giveAction("autoindent", autoIndentHandler);
giveAction("goto-line", gotoHandler);
giveAction("submit-assignment", submitHandler);

giveAction("compile", compileHandler);
giveAction("run", runHandler);
giveAction("run-input", runInputHandler);
giveAction("save-file", saveHandler);
giveAction("open-file", openFileHandler);
giveAction("new-file", newFileHandler);

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

function ssKeyHandler(foo, bar) {
    compiled = false;
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


