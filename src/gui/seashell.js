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
var editor = CodeMirror.fromTextArea(seashellEditor, {lineNumbers: true});

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
function runInputHandler() {
    var filePrompt = 'Name of input file: <input type="text" style="width: 3em"/>';
    editor.openDialog(filePrompt, function(query) {
            editor.setCursor(query-1, 0); });
}

function saveHandler() {
    var filePrompt = 'Save as: <input type="text" style="width: 3em"/>';
    editor.openDialog(filePrompt, function(query) {
            editor.setCursor(query-1, 0); });
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
giveAction("run-input", runInputHandler);
giveAction("save-file", saveHandler);
giveAction("open-file", openFileHandler);
giveAction("new-file", newFileHandler);
