function saveFile() {
    // contents is a \n-delimited string containing the text currently in the editor
    var contents = editor.getValue();
    var nameTag = document.getElementById('active_filename');
    nameTag.className="filename";

    // TODO save the file named currentFileName.
}

// returns the contents of currentFileName as a \n-delimited string 
function getFile() {
    // TODO
    return exampleCode;
}

// I'm not sure what this should look like. It should certainly be asynchronous.
// Maybe it should console_write(str) as it gets lines?
function runProgram() {
}

// eventually: parse clang output. Codemirror will let you jump around to arbitrary lines/positions
// and hilight bits of code. Should also probably be asynchronous.
function compileProgram() {
}
    
function setFileName(name) {
    var nameTag = document.getElementById('active_filename');
    nameTag.innerHTML=name;
    nameTag.className="filename";
}
var seashellEditor = document.getElementById('seashell');
//var txt = document.createTextNode("woohoo");
//seashellEditor.appendChild(txt);
var exampleCode = ['#include &lt;stdio.h&gt;',
'int main() {',
'    int i;',
'    for (i = 0; i &lt;3; i++) {',
'        printf("She sells C shells by the sea shore");',
'    }',
'    return(0);',
'}'].join('\n');

seashellEditor.innerHTML = exampleCode;

var editor = CodeMirror.fromTextArea(seashellEditor, {lineNumbers: true});
var welcomeMessage = 'Welcome to Seashell! Messages and program output will appear here.';
var currentFileName = 'foobar.c';
var console = CodeMirror(document.getElementById('console'), 
                            {value: welcomeMessage, 
                            readOnly: true, 
                            theme: 'dark-on-light'});
var compiled = false;
editor.on("change", mark_changed);

function mark_changed(instance, chobj) {
    compiled = false;
    var nameTag = document.getElementById('active_filename');
    nameTag.className = "filename status_edited"; 
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
    editor.openDialog(submitPrompt,
                        function(query) {
                            // TODO
                            console_write('Submitted file ' + currentFileName + '.');
                        });
}

function compileHandler() {
    saveFile();
    if (!compiled) {
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
                            // TODO problem with nullstring checking...
                            if (!query) {
                                console_write('Your file has been saved as ' + activeFileName + '.');
                            } else {
                                console_write('Your file has been saved as ' + query + '.');
                                setFileName(query);
                            }
                            saveFile();
                        });
}

function openFileHandler() {
    var filePrompt = 'File name: <input type="text" style="width: 3em"/>';
    editor.openDialog(filePrompt, 
                        function(query) {
                            // skip if no filename is specified. TODO figure out how to handle nullstrings
                            if (!query) {
                                return;
                            }
// TODO
//                            if (successful) {
                                console_write('Opened file ' + query + '.');
                                setFileName(query);
                                editor.setValue(getFile());
                                setFileName(query); // this is a kludge to stop filename from turning red
                                
//                            else {
//                                console_write('Failed to open the file ' + query + '.');
//                            }
                        });
}

function newFileHandler() {
    var filePrompt = 'Name of new file: <input type="text" style="width: 3em"/>';
    editor.openDialog(filePrompt, 
                        function(query) {
                            // skip if no filename is specified. TODO figure out how to handle nullstrings
                            if (!query) {
                                return;
                            }
// TODO
//                            if (successful) {
                                console_write('Creating file ' + query + '.');
                                setFileName(query);
                                editor.setValue('');
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

setFileName(currentFileName);
editor.focus();
