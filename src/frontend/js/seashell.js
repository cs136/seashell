/*--------------------------------------------------------------------
 Seashell
 Copyright (C) 2012-2013 Jennifer Wong, Marc Burns

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.

 Authors: Jennifer Wong, Marc Burns
 ---------------------------------------------------------------------*/

var ws; // websocket

var editor;
var ss_console;
var currentFile;
var fileList = []; // array of ssFiles open in the current session
var dir_listing = [];
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
    this.tab = $('<li class="filename" id="tab' + this.index + '"><span class="filename-text">' + name + '</span><span class="tabclose">x</span></li>');
    this.history = null;
    this.lastSaved = 'never';

    fileList[numberOfFiles] = this;
    numberOfFiles++;
}

/*

// I'm not sure what this should look like. It should certainly be asynchronous.
// Maybe it should console_write(str) as it gets lines?
// ensure currentFile is synced before calling this.
function runProgram() {
    ws.runFile(
            function(res) {
                if (res) {
                    window.ss_pipe_k = function() {
                        ss.getProgramOutput(function(bytes) {
                            if (bytes) {
                                console_write_noeol(bytes);
                                window.ss_pipe_k();
                            } else {
                                // do not poll too quickly.
                                window.setTimeout(window.ss_pipe_k, 500);
                            }
                        });
                    }
                    window.ss_term_k = function() {
                        ss.waitProgram(function(res) {
                            if (res) {
                                // Program terminated.
                                window.ss_pipe_k = function() {
                                };
                            } else {
                                window.ss_term_k();
                            }
                        });
                    }
                    window.ss_term_k();
                    window.ss_pipe_k();
                }
            }, currentFile.name);
}
*/

/* end code that touches ws.* functions */


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
    ss_console.scrollIntoView({line : ss_console.lineCount() - 1, ch: 0});
}

function console_write_noeol(str) {
    ss_console.setOption('readOnly', false);
    var newText = ss_console.getValue() + str;
    ss_console.setValue(newText);
    ss_console.setOption('readOnly', true);
}

function consoleFlushMsgs(response) {
	for (var i=0; i < response.messages.length; i++) {
		console_write(response.messages[i]);
	}
	response.messages = new Array();
	return response;
}

function makePrompt(str) {
    return str + ': <input class="seashell_input" type="text" style="width: 3em"/>';
}

function makeFilePrompt(str) {
    return str + ': <input class="seashell_input" type="text" style="width: 12em"/>';
}

function makeFilePrompt2(str, val) {
    return str + ': <input class="seashell_input" type="text" value="' + val + '" style="width: 12em"/>';
}

function getFileIndex(file_name) {
	for (var i = 0; i < numberOfFiles; i++) {
		if (fileList[i] != null && fileList[i].name == file_name) {
			return i;
		}
	}
	return -1;
}

/** as a general rule, the *Handler functions try to touch only
 * currentFile and the UI. non-handler functions never call handler
 * functions. **/

/** handlers for buttons that only affect the client-side **/
function toggleCommentLineHandler() {
    Format.commentLines();
}

function toggleCommentSelectionHandler() {
    Format.commentSelection();
}

function autoIndentHandler() {
    Format.formattedCode();
}

// codemirror lines are 0-indexed. This box takes lines as shown in the gutters
function gotoHandler() {
    editor.openDialog(makePrompt('Line'), function(query) {
        editor.setCursor(query - 1, 0);
    });
}

/** handlers for buttons that need to interact with the back-end **/

// javascript scoping is lots of "fun".
// http://stackoverflow.com/questions/4506240/understanding-the-concept-of-javascript-callbacks-with-node-js-especially-in-lo
function MakeFileCallback(i, flist) {
    return function(event) {
        event.stopPropagation();
        var req = openHandler(flist[i][1]);
		req.done(function (response) {
					$('.CodeMirror-dialog').remove();
				});
    };
}
function MakeDirCallback(i, flist) {
    return function(event) {
        console_write('Changing directory to ' + flist[i][1]);
        event.stopPropagation();
        getDirListing(flist[i][1])
			.done(function (response) {
				FileListToHTML(response.file_list);
			});
	};
}
function FileListToHTML(flist) {
    // not sure dir_listing needs to be kept around.
    dir_listing = flist;
    $('#file-list').html('');

    n_files = flist.length;
    for (var j = 0; j < n_files; j++) {
        if (dir_listing[j][0] == "f") {
            $('#file-list').append($('<li class="file">' + flist[j][1] + '</li>').click(
                    MakeFileCallback(j, flist)
            ));
        } else { // directory
            $('#file-list').append($('<li class="dir">' + flist[j][1] + '</li>').click(
                    MakeDirCallback(j, flist)
            ));
        }
    }
}

function openHandler(file_name) {
	var dfd = $.Deferred();
    if (file_name == "") {
		dfd.resolve();
		return dfd;
	}

    // if file is already open, don't open it twice
	var j = getFileIndex(file_name);
	if (j != -1) {
		if (currentFile && currentFile.name != fileList[j].name) {
			setTab(fileList[j]);
		}
		dfd.resolve();
		return dfd;
	} else {
		return getFile(file_name).done(function(response) {
			if (response.success) {
				var file = new ssFile(response.file_name, response.file_content);
				console_write('Opened file ' + response.file_name + '.');
				makeNewTab(file);
				setTab(file);
			} else {
				console_write('Failed to open the file ' + response.file_name + '.');
			}
			return response;
		});
	}
}

function openDialogHandler() {
    if ($('.CodeMirror-dialog').length) {
        $('.CodeMirror-dialog').remove();
    } else {
		editor.openDialog(
				makeFilePrompt("<ul id='file-list'></ul> File name"),
			function(name) {
				openHandler(name);
			}, {value: "asdf2"});
		getDirListing('/')
		   .done(function (response) {
			FileListToHTML(response.file_list);
			});
    }
}

/* fi is the index of some file in fileList. */
function closeDialogHandler(i) {

    editor.openConfirm('Are you sure? <button>Close file</button><button>Cancel</button>',
			[ function(foo) {
	console_write("closing file...");
    var j;
    if (fileList[i].name == currentFile.name) {

		if (numberOfFiles == 1) {
			// TODO properly handle the closing of one tab
			fileList[i].tab.hide();
			fileList[i] = null;
			editor.focus();
			return;
		}
        for (j = 0; j < numberOfFiles - 1; j++) {
            var offset = i - 1 - j;
            if (offset < 0) offset += numberOfFiles;
            if (fileList[offset] != null) {
                console_write('new file index should be ' + offset);
                setTab(fileList[offset]);
                fileList[i].tab.hide();
                fileList[i] = null;
                return;
            }
        }
    } else {
        console_write('closing index ' + i);
        fileList[i].tab.hide();
        fileList[i] = null;
        editor.focus();
        return;
    }
            }, undefined]);
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

function saveAndCompileHandler() {
	compileHandler(false);
}

function revertHandler() {
    editor.openConfirm('Are you sure? <button>Revert file</button><button>Cancel</button>',
			[function (unused) {
				if (!currentFile)
					return;
				var req = revertFile(currentFile.name);
				req
				.then(
					function (response) {
						if (response.success) {
							var j = getFileIndex(response.file_name);
							if (j == -1) {
								console_write("Cannot revert the file " + response.file_name + " because it is not currently open.");
								response.success = false;
							} else {
								fileList[j].content = response.file_content;
								if (currentFile.name == response.file_name) {
									editor.setValue(response.file_content);
								}
								console_write("Reverted file " + response.file_name);
							}
						}
						return response;
					})
				.then(null, function (response) {
					console_write("Failed to revert the file " + response.file_name);
					consoleFlushMsgs(response);
				});
			}, undefined]);
}

function saveHandler(show_output) {
    if (!currentFile) {
		var dfd = $.Deferred();
		dfd.reject();
        return  dfd;
	}

    // editor.getValue() is a \n-delimited string containing the text currently in the editor
    currentFile.content = editor.getValue();
	currentFile.history = editor.getHistory();

	var req = saveFile(currentFile.name, currentFile.content, currentFile.history);
	// return after saveHandler2 is complete. saveHandler2 modifies response.
	return req.then(saveHandler2, consoleFlushMsgs).then(
		function (response) {
			if (show_output) {
				return consoleFlushMsgs(response);
			} else {
				response.messages = new Array();
				return response;
			}
		});
}

function saveHandler2(response) {
	var dfd = $.Deferred();
	var j = getFileIndex(response.file_name);

	if (j == -1) {
		response.success = false;
		response.messages.push('Tried to save a file that is no longer open!');
	} else if (response.success) {
		fileList[j].lastSaved = (new Date()).toLocaleTimeString();
		response.messages.push('Your file has been saved as ' + fileList[j].name + '.');

		if (currentFile.index == j) {
			mark_unchanged();
			$('#time-saved').text(currentFile.lastSaved); 
		}
	} else {
		response.messages.push('Could not save file ' + response.file_name);
	}

	if (response.success) {
		dfd.resolve(response);
	} else {
		dfd.reject(response);
	}
	return dfd;
}

/** this is really more of a commitHandler **/
/** may decide to save file under a different name. **/
function saveAsHandler() {
	if (! currentFile) {
		console_write("No current file.");
		return;
	}

    editor.openDialog(makeFilePrompt2('Commit as', currentFile.name),
            function(query) {
				if (!currentFile) {
					console_write("No current file.");
					return;
				}

                if (query != "") {
                    currentFile.name = query;
                    currentFile.tab.children(".filename-text")[0]
							.innerHTML = query;
                } else {
                    console_write("Blank filename! Saving with old name.");
                }

				// editor.getValue() is a \n-delimited string containing the text currently in the editor
				currentFile.content = editor.getValue();
				currentFile.history = editor.getHistory();

				commitFile(currentFile.name, currentFile.content, currentFile.history)
				.then(function(response) {
						if (response.success) {
							console_write("You have commited the file " + response.file_name); 
						}
					}, 
					consoleFlushMsgs);
            });
}

function runHandler() {
	return runProgram(currentFile.name)
		.then( function (response) {
			console_write("Running program.");
			return response;
		},
		consoleFlushMsgs);
}

function compileHandler(show_output) {
	var req = saveHandler(show_output); // ws_saveFile promise filtered through saveHandler2()

	// done() means server saved the file. 
	// fail() means server failed to save.
	return req.then(function (response) { 
						return compileProgram(response.file_name); 
					}, consoleFlushMsgs)
		.then( function (response) {
			if (show_output) {
				if (!response.success) { // compile errors or warnings
					result_cb(response.error_list);
					console_write("Compile failed.");
					clangMessages.highlightErrors();
				} else {
					console_write("Compiled successfully.");
				}
			}
			var dfd = $.Deferred();
			if (response.success)
				return dfd.resolve(response);
			else
				return dfd.reject(response);
		},
		consoleFlushMsgs);
}

function makeNewTab(file) {
    if (numberOfFiles == 1) {
        file.tab.addClass("status_active");
    }

    $("#filelist").append(file.tab);
    file.tab.click(function() {
        setTab(file);
    });
    $("#tab" + file.index + " .tabclose").click(
            function(event) {
                event.stopPropagation();
                closeDialogHandler(file.index);
            });
}

function setFirstTab(file) {
    editor.setValue(file.content);
    editor.clearHistory();
    currentFile = file;
    $('#time-saved').text(currentFile.lastSaved);
    mark_unchanged();
}

/** will update currentFile as well **/
function setTab(file) {
    // save previously open tab before opening new one
    return saveHandler(false).done( function (response) {
    editor.clearHistory();

    // set active tab
    $(".status_active").removeClass("status_active");
    file.tab.addClass("status_active");
    editor.focus();
    editor.setValue(file.content);
    if (file.history != null) {
        editor.setHistory(file.history);
    }
    currentFile = file;
    $('#time-saved').text(currentFile.lastSaved);
    mark_unchanged();
			});
}

function switchTabHandler(forwards) {
    i = currentFile.index;
    a = 1;
    if (!forwards) a = -1;

    for (j = 0; j < numberOfFiles - 1; j++) {
        var offset = (i + a) + a * j;
        if (offset < 0) offset += numberOfFiles;
        if (offset >= numberOfFiles) offset -= numberOfFiles;
        if (fileList[offset] != null) {
            console_write('new file index should be ' + offset);
            setTab(fileList[offset]);
            return;
        }
    }

}

function submitHandler() {
    editor.openDialog(makePrompt('Assignment ID'),
            function(query) {
                // TODO
                console_write('Submitted file ' + currentFile.name + ', ' + (new Date()).toLocaleTimeString() + '.');
            });
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
    var new_width = parseInt($('#editor_width').val());
	var new_fontsize = parseInt($('#editor_font').val());
	$('.CodeMirror').css('font-size', new_fontsize);

	// needed for heightAtLine to be correct
	editor.refresh();

    if (!isNaN(new_height)) {
		var line_offset = parseInt(editor.heightAtLine(0, "local"));
		var line_height = parseInt(editor.heightAtLine(1, "local")) - line_offset;
        editor.setSize(null, 2*line_offset + (new_height + 1) * line_height);
    }
    if (!isNaN(new_width)) {
		var char_pos = editor.charCoords({line:1,ch:1}, "local");
		var char_width = char_pos.right - char_pos.left;
		// TODO
		editor.setSize(50 + char_pos.left + new_width * (char_width), null);
    }

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

    CodeMirror.commands.save = function() {saveHandler(true);};
    var delay;
    var delayTime = 1000;
    editor = CodeMirror.fromTextArea($("#seashell")[0],
            {//value: currentFile.content,

                lineNumbers: true,
                tabSize: defaultTabSize,
                mode: "text/x-csrc",
                tabMode: "shift", // todo uncomment?
                gutters: ["CodeMirror-lint-markers"],
                matchBrackets: true,
                autofocus: true,
                lintWith: {
                    "getAnnotations": CodeMirror.remoteValidator,
                    "async": true,
                    "check_cb": check_syntax
                }});
    editor.on('change', function() {
          clearTimeout(delay);
          delay = setTimeout(saveAndCompileHandler, delayTime);
        });
    editor.setOption('extraKeys',
            {"Ctrl-O": function(cm) {
                openDialogHandler();
            },
                "Ctrl-N": function(cm) {
                    newFileHandler();
                },
                "Ctrl-I": function(cm) {
                    autoIndentHandler()
                },
                "Ctrl-J": function(cm) {
                    gotoHandler();
                },
                "Ctrl-Enter": function(cm) {
                    runHandler();
                },
                "Ctrl-Left": function() {
                    switchTabHandler(false);
                },
                "Ctrl-Right": function() {
                    switchTabHandler(true);
                }
            });

    // openFile("foobar.c") without a setTab(file)
    getFile(function(data) {
        if (data) {
            var file = new ssFile(defaultFileName, data);
            makeNewTab(file);
            setFirstTab(file);
            console_write('Opened file ' + defaultFileName + '.');
        } else {
            console_write('Failed to open the file ' + defaultFileName + '.');
        }
    }, defaultFileName);
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

    $("#undo").click(function() {
        editor.undo();
    });
    $("#redo").click(function() {
        editor.redo();
    });

    $("#commentLine").click(function() {
        toggleCommentLineHandler();
    });
    $("#commentSelection").click(function() {
        toggleCommentSelectionHandler();
    });
    $("#autoindent").click(autoIndentHandler);
    $("#goto-line").click(gotoHandler);
    $("#submit-assignment").click(submitHandler);

    $("#clear-console").click(function() {
        ss_console.setValue('')
    });
    $("#compile").click(saveAndCompileHandler);
    $("#run").click(runHandler);
    $("#run-input").click(runInputHandler);
    $("#saveas-file").click(saveAsHandler);
    $("#revert-file").click(revertHandler);
    $("#open-file").click(openDialogHandler);
    $("#new-file").click(newFileHandler);

    /** settings glue **/
    $("#overlay").hide();
    $("#settings").hide();
    $("#settings").change(configureEditor);
    $("#settingsToggle").click(showSettings);

    var editor_dimensions = editor.getScrollInfo();

	var line_offset = parseInt(editor.heightAtLine(0, "local"));
	var line_height = parseInt(editor.heightAtLine(1, "local")); - line_offset;

    var editor_height = parseInt($("#codeArea").css("height"));
    var height_in_lines = Math.ceil((editor_dimensions.clientHeight - 0*line_offset) / editor.defaultTextHeight()) - 1;
    $("#editor_height").val(String(height_in_lines));

	var char_pos = editor.charCoords({line:1,ch:1}, "local");
	var char_width = char_pos.right - char_pos.left;
	var editor_width = parseInt($('#codeArea').css('width'));
	var width_in_chars = Math.round(editor_dimensions.clientWidth / char_width - 1);
	$('#editor_width').val(String(width_in_chars));

	var font_size = parseInt($(".CodeMirror").css("font-size"));
	$('#editor_font').val(String(font_size));

    $("#help").hide();
    $("#helpToggle").toggle(function() {
        $("#help").show();
    }, function() {
        $("#help").hide();
    });
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


function seashellInit() {
	var creds = read_login_credentials();
	if (creds) {
		ws = new SeashellWebsocket("ws://" + creds.host + ":" + creds.port, creds.key);
		setUpUI();
	}
}

