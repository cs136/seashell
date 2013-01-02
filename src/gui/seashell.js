var editor;
var ss_console;
var currentFile;
var fileList = []; // array of ssFiles open in the current session
var numberOfFiles = 0;
var compiled = false;

var defaultFileName = "foobar.c";
var defaultTabSize = 2;

var vimBindingsLoaded = false;
var emacsBindingsLoaded = false;

// creates a new ssFile that fileList will be aware of
function ssFile(name, content) {
    this.name = name;
    this.index = numberOfFiles;
    this.content = content;
    this.tab = $('<li class="filename">' + name + '</li>');
    this.history = null;
    this.lastSaved = 'never';

    fileList[numberOfFiles] = this;
    numberOfFiles++;
}

// I'm not sure what this should look like. It should certainly be asynchronous.
// Maybe it should console_write(str) as it gets lines?
// ensure currentFile is synced before calling this.
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
    }}, currentFile.content);
}

// eventually: parse clang output. Codemirror will let you jump around to arbitrary lines/positions
// and hilight bits of code. Should also probably be asynchronous.
function compileProgram() {
}
    
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

/** as a general rule, the *Handler functions try to touch only 
 * currentFile and the UI. **/

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
    editor.autoIndentRange(from, to);
}

// codemirror lines are 0-indexed. This box takes lines as shown in the gutters
function gotoHandler() {
    editor.openDialog(makePrompt('Line'), function(query) {
            editor.setCursor(query-1, 0); });
}

/** handlers for buttons that need to interact with the back-end **/

function saveFile() {
    // editor.getValue() is a \n-delimited string containing the text currently in the editor
    currentFile.content = editor.getValue();
    currentFile.history = editor.getHistory();

    ss.saveFile(
      function(res) {
        if(!res) {
          alert("Could not save file. Please try again.");
        } else {
            mark_unchanged();
            currentFile.lastSaved = (new Date()).toLocaleTimeString();
            $('#time-saved').text(currentFile.lastSaved);
            //console_write('Your file has been saved as ' + currentFile.name + '.');
        }
      },
      currentFile.name,
      currentFile.content);
}

/** may decide to save file under a different name. **/
function saveHandler() {
    editor.openDialog(makeFilePrompt('Save as'), 
            function(query) {
                if (query != "") {
                    currentFile.name = query;
                    currentFile.tab.text(query);
                } else {
                    console_write("Blank filename! Saving with old name.");
                }
                saveFile();
            });
}

// applies k to the contents of name as a \n-delimited string 
function getFile(k, name) {
    ss.loadFile(k, name);
}

function openFileHandler() {
    editor.openDialog(
            makeFilePrompt('File name'), 
            function(name) {
                if (name == "") return;

                // if file is already open, don't open it twice
                for (var i=0; i<numberOfFiles; i++) {
                    if (fileList[i].name == name) {
                        getFile(function(data) {
                                if (fileList[i].name == currentFile.name) {
                                    editor.setValue(data);
                                } else {
                                    fileList[i].content = data;
                                }
                                setTab(fileList[i]);
                            }, name);

                        return;
                    }
                }

                getFile(function(data) {
                  if(data) {
                    var file = new ssFile(name, data);
                    makeNewTab(file);
                    setTab(file);
                    console_write('Opened file ' + name + '.');
                  } else {
                    console_write('Failed to open the file ' + name + '.');
                  }}, name);

            });
}

function newFileHandler() {
    editor.openDialog(
            makeFilePrompt('Name of new file'), 
            function(query) {
                if (query == "") return;
                    var successful = true; // TODO
                    if (successful) {
                        console_write('Creating file ' + query + '.');
                        var file = new ssFile(query, "");
                        makeNewTab(file);
                        setTab(file);
                    } else {
                        console_write('Failed to create the file ' + query + '.');
                    }
            });
}

function makeNewTab(file) {
    if (numberOfFiles == 1) {
        file.tab.addClass("status_active");
    }

    $("#filelist").append(file.tab);
    file.tab.click(function() { setTab(file); });
}

function setFirstTab(file) {
    editor.setValue(file.content);
    currentFile = file;
    $('#time-saved').text(currentFile.lastSaved);
    mark_unchanged();
}

/** will update currentFile as well **/
function setTab(file) {
    // save previously open tab before opening new one
    saveFile(); 
    editor.clearHistory();
    
    // set active tab
    $(".status_active").removeClass("status_active");
    file.tab.addClass("status_active");
    editor.setValue(file.content);
    if (file.history != null) {
        editor.setHistory(file.history);
    }
    currentFile = file;
    $('#time-saved').text(currentFile.lastSaved);
    mark_unchanged();
}

function submitHandler() {
    editor.openDialog(makePrompt('Assignment ID'), 
            function(query) {
                // TODO
                console_write('Submitted file ' + currentFile.name + '.');
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
    saveFile();
    runProgram();
}

function runInputHandler() {
    editor.openDialog(makeFilePrompt('Name of input file'), 
            function(query) {
                // TODO run
            });
}

// reads off the form in #settings.
function configureEditor() {
    var new_height = parseInt($('#editor_height').val());
    //var new_width = parseInt($('#editor_width').val());
    if (!isNaN(new_height)) {
        editor.setSize(null, new_height * (editor.defaultTextHeight()+1));
    }
    /*
    if (!isNaN(new_width)) {
        editor.setSize(new_width * (editor.defaultTextHeight()+1), null);
    }
    */
    var editor_mode = $('#editor_mode input').filter(':checked').val();
    if (editor_mode == "vim" && !vimBindingsLoaded) {
        jQuery.getScript("codemirror/keymap/vim.js", 
                function(script, textStatus, jqXHR) {
                    console_write("Setting editor mode to " + editor_mode);
                    editor.setOption('keyMap', editor_mode);
                    vimBindingsLoaded = true;
                });
    } else if (editor_mode == "emacs" && !emacsBindingsLoaded) {
        jQuery.getScript("codemirror/keymap/emacs.js", 
                function(script, textStatus, jqXHR) {
                    console_write("Setting editor mode to " + editor_mode);
                    editor.setOption('keyMap', editor_mode);
                    emacsBindingsLoaded = true;
                });
    } else {
        console_write("Setting editor mode to " + editor_mode);
        editor.setOption('keyMap', editor_mode);
    }
                    
    var tab_width = $('#tab-width option').filter(':selected').val();
    console_write("Tab-width changed to " + tab_width);
    editor.setOption('tabSize', tab_width);
    editor.setOption('indentUnit', tab_width);

    var use_tabs = $('#use-spaces').is(':checked');
    editor.setOption('indentWithTabs', use_tabs);
}

/** diagnostic utility functions **/
function printSettings() {
    console_write("Using editor " + editor.getOption('keyMap'));
    console_write("Using tab-width " + editor.getOption('tabSize'));
    if (editor.getOption('indentWithTabs')) {
        console_write("Indenting with tabs");
    } else {
        console_write("Indenting with spaces");
    }
}

function hoboFile(name) {
    var exampleCode = ['#include <stdio.h>',
    'int main() {',
    '    int i;',
    '    for (i = 0; i <3; i++) {',
    '        printf("She sells C shells by the sea shore");',
    '    }',
    '    return(0);',
    '}'].join('\n');
    return new ssFile(name, exampleCode);
}
/** end diagnostic utility functions **/

function setUpUI() {
    /** create editor and console **/

    CodeMirror.commands.save = saveFile;
    editor = CodeMirror.fromTextArea($("#seashell")[0], 
                {//value: currentFile.content,
                lineNumbers: true,
                tabSize: defaultTabSize});
    editor.setOption('extraKeys', 
            {"Ctrl-O": function(cm) {openFileHandler();},
             "Ctrl-N": function(cm) {newFileHandler();},
             "Ctrl-I": function(cm) {autoIndentHandler()},
             "Ctrl-J": function(cm) {gotoHandler();},
             "Ctrl-Enter": function(cm) {runHandler();},
             "Ctrl-Left": false, // TODO
             "Ctrl-Right": false // TODO
             });

    // openFile("foobar.c") without a setTab(file)
    getFile(function(data) {
      if(data) {
        var file = new ssFile(defaultFileName, data);
        makeNewTab(file);
        setFirstTab(file);
        console_write('Opened file ' + defaultFileName + '.');
      } else {
        console_write('Failed to open the file ' + defaultFileName + '.');
      }}, defaultFileName);
    //

    var welcomeMessage = 'Welcome to Seashell! Messages and program output will appear here.\n';
    ss_console = CodeMirror($('#console')[0],
                                   {value: welcomeMessage, 
                                   readOnly: true, 
                                   theme: 'dark-on-light'});
    // 10 cols high by default
    ss_console.setSize(null, ss_console.defaultTextHeight() * 10);
    editor.focus();

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

    /** settings glue **/
    $("#overlay").hide();
    $("#settings").hide();
    $("#settings").change(configureEditor);
    $("#settingsToggle").click(showSettings);

    var editor_height = parseInt($("#codeArea").css("height"));
    var height_in_lines = Math.round(editor_height / editor.defaultTextHeight() - 1) - 1;
    $("#editor_height").val(String(height_in_lines));

    $("#help").hide();
    $("#helpToggle").toggle(function() {$("#help").show();}, function() {$("#help").hide();});
}

function showSettings() {
    $("#overlay").show();
    $("#settings").show();
    $("#overlay").click(hideSettings);
}

function hideSettings() {
    $("#overlay").hide()
    $("#settings").hide();
    editor.focus();
}

/** initialize api. **/
seashell_new(
  function(ss) {
    window.ss = ss;
    ss.authenticate(
      function(res) {
        if(!res) {
          alert("Couldn't authenticate as ctdalek!");
          // TODO maybe make failure pretty? Haha.
        }
        setUpUI();
      },
      "ctdalek", "exterminate");
  },
  function(err) {
    alert("Error initializing API: " + err);
  });
