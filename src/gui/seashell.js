// I'm not sure what this should look like. It should certainly be asynchronous.
// Maybe it should console_write(str) as it gets lines?
function runProgram() {
  ss.runFile(
    function(res) {
      if(res) {
        window.ss_pipe_k = function() {
          ss.getProgramOutput(function(bytes) {
            if(bytes) {
              console_write_noeol(bytes);
              window.ss_pipe_k();
            } else {
              /* do not poll too quickly. */
              window.setTimeout(window.ss_pipe_k, 500);
            }
          });
        }
        window.ss_term_k = function() {
          ss.waitProgram(function(res) {
            if(res) {
              /* Program terminated. */
              window.ss_pipe_k = function(){};
            } else {
              window.ss_term_k();
            }
          });
        }
        window.ss_term_k();
        window.ss_pipe_k();
    }}, editor.getValue());
}

// eventually: parse clang output. Codemirror will let you jump around to arbitrary lines/positions
// and hilight bits of code. Should also probably be asynchronous.
function compileProgram() {
}
    
function setFileName(name) {
    mark_unchanged();
    $(".status_active").text(name);
    currentFileName=name;
}
//var txt = document.createTextNode("woohoo");
//seashellEditor.appendChild(txt);
var exampleCode = ['#include <stdio.h>',
'int main() {',
'    int i;',
'    for (i = 0; i <3; i++) {',
'        printf("She sells C shells by the sea shore");',
'    }',
'    return(0);',
'}'].join('\n');

$("#seashell").text(exampleCode);

var editor = CodeMirror.fromTextArea($("#seashell")[0], {lineNumbers: true});
var welcomeMessage = 'Welcome to Seashell! Messages and program output will appear here.\n';
var currentFileName = 'foobar.c';
var ss_console = CodeMirror($('#console')[0],
                               {value: welcomeMessage, 
                               readOnly: true, 
                               theme: 'dark-on-light'});
var compiled = false;
editor.on("change", mark_changed);

function mark_changed(instance, chobj) {
    compiled = false;
    $(".status_active").addClass("status_edited");
}

function mark_unchanged() {
    $(".status_active").removeClass("status_edited");
}

function console_write(str) {
    ss_console.setOption('readOnly', false);
    var newText = ss_console.getValue() + str + '\n';
    ss_console.setValue(newText);
    ss_console.setOption('readOnly', true);
}

function console_write_noeol(str) {
    ss_console.setOption('readOnly', false);
    var newText = ss_console.getValue() + str;
    ss_console.setValue(newText);
    ss_console.setOption('readOnly', true);
}

function makePrompt(str) {
    return str + ': <input type="text" style="width: 3em"/>';
}

function makeFilePrompt(str) {
    return str + ': <input type="text" style="width: 12em"/>';
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
    editor.openDialog(makePrompt('Line'), function(query) {
            editor.setCursor(query-1, 0); });
}

/** handlers for buttons that need to interact with the back-end **/

function saveFile() {
    // editor.getValue() is a \n-delimited string containing the text currently in the editor

    ss.saveFile(
      function(res) {
        if(!res) {
          alert("Could not save file. Please try again.");
        } else {
            mark_unchanged();
            console_write('Your file has been saved as ' + currentFileName + '.');
        }
      },
      currentFileName,
      editor.getValue());
}

function saveHandler() {
    editor.openDialog(makeFilePrompt('Save as'), 
                        function(query) {
                            // TODO problem with nullstring checking...
                            if (query) {
                                setFileName(query);
                            }
                            saveFile();
                        });
}

// applies k to the contents of currentFileName as a \n-delimited string 
function getFile(k) {
    ss.loadFile(k, currentFileName);
}

function openFileHandler() {
    editor.openDialog(makeFilePrompt('File name'), 
                        function(query) {
                            // skip if no filename is specified. TODO figure out how to handle nullstrings
                            if (!query) {
                                return;
                            }

                            getFile(function(val) {
                              if(val) {
                                editor.setValue(val);
                                console_write('Opened file ' + query + '.');
                                setFileName(query);
                                mark_unchanged();
                              } else {
                                console_write('Failed to open the file ' + query + '.');
                              }});
                        });
}

function newFileHandler() {
    editor.openDialog(makeFilePrompt('Name of new file'), 
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

function submitHandler() {
    editor.openDialog(makePrompt('Assignment ID'),
                        function(query) {
                            // TODO
                            console_write('Submitted file ' + currentFileName + '.');
                        });
}

function compileHandler() {
    saveFile();
    /*if (!compiled) {
        // TODO compile file
        compiled = true;
        console_write('Done compiling.');
    } else {
        console_write('Already compiled.');
    }*/
}

function runHandler() {
    /*if (!compiled) {
        console_write('The source file was modified since the last compile. Compiling first...');
        compileHandler();
    }*/
    // TODO run
    runProgram();
}

function runInputHandler() {
    editor.openDialog(makeFilePrompt('Name of input file'), 
                        function(query) {
                            // TODO run
                        });
}

/** initialize api. **/
seashell_new(
  function(ss) {
    window.ss = ss;
    ss.authenticate(
      function(res) {
        if(!res) {
          alert("Couldn't authenticate as ctdalek!");
        }
      },
      "ctdalek", "exterminate");
  },
  function(err) {
    alert("Error initializing API: " + err);
  });

/** attach actions to all the buttons. **/

$("#undo").click(function() {editor.undo();});
$("#redo").click(function() {editor.redo();});
        
$("#comment").click(function() {toggleCommentHandler(true);});
$("#uncomment").click(function() {toggleCommentHandler(false);});
$("#autoindent").click(autoIndentHandler);
$("#goto-line").click(gotoHandler);
$("#submit-assignment").click(submitHandler);

$("#clear-console").click(function() {ss_console.setValue('')});
$("#compile").click(compileHandler);
$("#run").click(runHandler);
$("#run-input").click(runInputHandler);
$("#save-file").click(saveHandler);
$("#open-file").click(openFileHandler);
$("#new-file").click(newFileHandler);

setFileName(currentFileName);
editor.focus();
