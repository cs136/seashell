function giveAction(elid,act) {
    document.getElementById(elid).onclick=act;
}
var seashellEditor = document.getElementById('seashell');
//var txt = document.createTextNode("woohoo");
//seashellEditor.appendChild(txt);
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
    var submitPrompt = 'Name of input file: <input type="text" style="width: 3em"/>';
    editor.openDialog(submitPrompt, function(query) {
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
