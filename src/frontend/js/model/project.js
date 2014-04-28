"use strict";

/*
 * Seashell's front-end.
 * Copyright (C) 2013-2014 The Seashell Maintainers.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
var currentFiles = null;
var currentFile = null;
var currentProject = null;
var currentErrors = [];
var currentPID = null;
var saveTimeout = null;
var settings = null;

/**
 * Updates the list of projects.
 */
function updateListOfProjects() {
  var promise = socket.getProjects();

  /** Fill the selector */
  promise.done(function(projects) {
    var projects_tag = $("#projects_list");

    projects_tag.empty();
    for (var i = 0; i < projects.length; i++) {
      projects_tag.append(
        $("<option>").attr("value", projects[i]).text(projects[i]));
    }
  }).fail(function(){
    displayErrorMessage("List of projects could not be updated.");
  });
}

/**
 * Updates the list of marmoset projects in the submit dialog.
 */
// TODO: ensure the student submits with the right filename using fnames array
function updateMarmosetProjects() {
    $.get("https://www.student.cs.uwaterloo.ca/~cs136/cgi-bin/project-list.cgi",
        function(data){
            var marmoset_tag = $("#marmoset_project");
            var $xml = $(data);
            var $rows = $xml.find("row");
            var assns = $.map($rows.find("field[name=\"project_number\"]"), function (x) {return x.textContent;});
            // var fnames = $.map($rows.find("field[name=\"title\"]"), function (x) {return x.textContent;});
            for(var i = 0; i < assns.length; i++){
                marmoset_tag.append(
                    $("<option>").attr("value", assns[i]).text(assns[i]));
                }
            });
}


/**
 * Saves everything in the current project.
 * @return {Promise} JQuery promise when all files are saved.
 */
function projectSave() {
  var promises = [];
  for (var file in currentFiles) {
    if ("document" in currentFiles[file]) {
      promises.push(saveFile(file));
    }
  }

  return $.when.apply(null, promises)
    .done(function () {
      window.clearTimeout(saveTimeout);
      saveTimeout = window.setTimeout(projectSave, 10000);
    });
}

/**
 * Saves a file.
 * @param {String} file Name of file to save.
 * @return {Promise} promise JQuery promise when file is saved.
 */
function saveFile(file) {
  return socket.writeFile(currentProject, file, currentFiles[file].document.getValue()).fail(function() {
    displayErrorMessage(file+" could not be saved.");
  });
}

/**
 * Creates a new file.
 * @param {String} file Name of file to create.
 * @return {Promise} promise JQuery promise when file is saved.
 */
function createFile(file) {
  return socket.newFile(currentProject, file).fail(function() {
    displayErrorMessage(file+" could not be created.");
  });
}

/**
 * Closes the project.
 *
 * @param {Boolean} save Save project on close or not.
 */
function projectClose(save) {
  if(currentProject) socket.unlockProject(currentProject);
  if (save)
    projectSave();
  window.clearTimeout(saveTimeout);
  saveTimeout = null;
  currentFiles = null;
  currentProject = null;
  currentErrors = [];
  /** Delete the list of files. */
  $(".file-entry").remove();
  /** OK, hide project - hide files */
  $(".show-on-null-project").removeClass("hide");
  $(".hide-on-null-project").addClass("hide");
  $(".hide-on-null-file").addClass("hide");
  $(".show-on-null-file").removeClass("hide");
  $("#project-menu").text("No project open");
}

/**
 * Closes the current file. 
 *
 * @param {Boolean} save Save file on close or not.
 */
function fileClose(save) {
  if (currentFile) {
    if (save)
      saveFile(currentFile);

    /** Swap out the document. */
    editor.swapDoc(new CodeMirror.Doc("", "text/x-csrc"));
    // remove rename click event and attach file open click event
    $(".active").children().unbind("click")
      .click(fileNavigationClickHandler);
    /** Remove the active class from the links. */
    $(".file-entry").removeClass("active");
    /** Hide the editor. */
    $(".hide-on-null-file").addClass("hide");
    $(".show-on-null-file").removeClass("hide");
    /** Unset the current file. */
    currentFile = null;
  }
}

/**
 * Deletes a file.
 * @param {String} file Name of file.
 */
function fileDelete(file) {
  if (currentFile == file) {
    fileClose(false);
  }
  socket.deleteFile(currentProject, file).done(function () {
    currentFiles[file].tag.remove();
    delete currentFiles[file];
  }).fail(function() {
    displayErrorMessage(file+" could not be deleted.");
  });
}


/**
 * Opens a file.
 * @param {String} file Name of file.
 */
function fileOpen(name) {
  function rest( ) {
    fileClose();
    /** Show file related stuff. 
     *  THIS must happen before documents swap as something
     *  breaks on rendering (observed on Firefox 26).*/
    $(".hide-on-null-file").removeClass("hide");
    $(".show-on-null-file").addClass("hide");
    /** OK, set file contents. */
    editorDocument(currentFiles[name].document);
    /** Add the active class to this link. */
    currentFiles[name].tag.addClass("active")
      .children().click(fileRename);
    /** Load the file. */
    currentFile = name;
  }

  if (! ("document" in currentFiles[name])) {
    socket.readFile(currentProject, name).done(function (contents) {
      // TODO: Custom file types that are not C source files.
      // Consult lib/codemirror/mode and stuff.
      currentFiles[name].document = new CodeMirror.Doc(contents, "text/x-csrc");
      currentFiles[name].document.on("change",
        function () {
          currentErrors = currentErrors.filter(function (error) {return error[1] != name;});
        });
      rest();
    }).fail(function () {
      displayErrorMessage(name+" could not be read.");
    }); 
  } else {
    rest();
  }
}

/**
 * Creates a file.
 * @param {String} file Name of file to create.
 * @return {Promise} JQuery promise when the file is created.
 */
function fileNew(name) {
  /** (Lazily) create the file. */
  if (name in currentFiles) {
    // This should never happen,
    // as a project should only be open in one Seashell instance
    // at any time.  Though that still has to be handled.
    displayErrorMessage(name+" already exists.");
  } else {
    // TODO: Custom file types.
    currentFiles[name] = {"document": CodeMirror.Doc("/**\n * File: " + name + "\n * Enter a description of this file.\n*/", "text/x-csrc")};
    currentFiles[name].document.on("change",
        function () {
          currentErrors = currentErrors.filter(function (error) {return error[1] != name;});
        });
    // TODO: Proper new file call.
    createFile(name).done(function () {;
      fileNavigationAddEntry(name);
      fileOpen(name)})
    .fail(function () {
      // TODO: Error handling.
    });
  }
}

/**
 * Creates a file entry in the UI.
 * @param {String} file Name of file for which a navigation link
 * should be added.
 */
function fileNavigationAddEntry(file) {
  // Bind the tag.
  var tag = $("<li>");
  tag.addClass("file-entry")
     .append(
        $("<a>").text(file).append(
          $("<span>").addClass("pull-right")
            .addClass("glyphicon").addClass("glyphicon-trash")
            .on("click", fileNavigationTrashHandler(file)))
          .click(fileNavigationClickHandler));
  // Add the tag.
  $("#file-navigator").append(tag);
  // Save the tag for easy access.
  currentFiles[file].tag = tag; 
}

/* click event handler for file navigation items */
function fileNavigationClickHandler(event) {
  /** Only handle click events for the A tag itself, not for the span inside it. */
  if(event.target == this)
    fileOpen($(this).text());
}

function fileNavigationTrashHandler(file) {
  return function(event) {
    event.stopPropagation();
    deleteFileDialog(file);
  };
}

/**
 * Locks a new project, opens it, and sets the current project.
 * @param {String} project Name of project to open.
 */
function projectOpen(name) {
  var lockPromise = socket.lockProject(name);
  var openNoLock = function(){ projectOpenNoLock(name); };
  lockPromise.done(openNoLock).fail(function(res){
    if(res == "locked"){
        displayConfirmationMessage("Project is locked by another browser instance",
            "Press OK to forcibly unlock the project, or Cancel to abort.",
            function(){
                var forceLockPromise = socket.forceLockProject(name);
                forceLockPromise.done(openNoLock).fail(function(){
                    displayErrorMessage("Project "+name+" could not be unlocked.");
                });
            });
    }else{
        displayErrorMessage("Project "+name+" failed to lock.");
    }
  });
}

/**
 * Opens a new project and sets the current project (without locking).
 * @param {String} project Name of project to open.
 */
function projectOpenNoLock(name) {
  var promise = socket.listProject(name);

  /** Update the list of files. */
  promise.done(function(files) {
    projectClose(true);
    currentFiles = {};
    currentProject = name;
    currentErrors = [];
    saveTimeout = window.setTimeout(projectSave, 10000);

    $("#project-menu").text(name);

    for (var i = 0; i < files.length; i++) {
      /** Lazily load the documents later. */
      currentFiles[files[i]] = {};
      /** Create a entry for it. */
      fileNavigationAddEntry(files[i]);
    }
    /** OK, show project - hide files */
    $(".show-on-null-project").addClass("hide");
    $(".hide-on-null-project").removeClass("hide");
    $(".hide-on-null-file").addClass("hide");
    $(".show-on-null-file").removeClass("hide");
    /** Refresh the console. */
    consoleRefresh();
  }).fail(function(){
    displayErrorMessage("Project "+name+" could not be opened.");
  });
}

/** 
 * Creates and opens a project.
 * @param {String} project Name of project to create.
 */
function projectNew(name) {
  var promise = socket.newProject(name);

  /** Open it. */
  promise.done(function() {
    projectOpen(name);
  }).fail(function() {
    displayErrorMessage("Project "+name+" could not be created.");
  });
}

/**
 * Delete and closes the current project.
 */
function projectDelete() {
  var promise = socket.deleteProject(currentProject);

  /** Deal with it. */
  promise.done(function() {
    projectClose(false);
  }).fail(function() {
    displayErrorMessage("Project could not be deleted.");
  });

  return promise;
}

/**
 * Compiles the current project.
 */
function projectCompile() {
  var save_promise = projectSave();
  var promise = $.Deferred();

  save_promise.done(function () {
    socket.compileProject(currentProject, promise);

    /** Helper function for writing errors. */
    function writeErrors(errors) {
      consoleWrite("*** clang produced the following messages:");
      for (var i = 0; i < errors.length; i++) {
        var error = errors[i][0];
        var file = errors[i][1];
        var line = errors[i][2];
        var column = errors[i][3];
        var message = errors[i][4];

        consoleWrite(sprintf("*** %s:%d:%d: %s: %s",
            file, line, column,
            error ? "error" : "warning",
            message));
      }
    }

    /** Deal with it. */
    promise.done(function(messages) {
      // Save the messages.
      currentErrors = messages;
      // Write it to the console
      writeErrors(currentErrors);
      // Lint
      editorLint();
    }).fail(function(result) {
      if (Array.isArray(result)) {
        // Log
        consoleWrite(sprintf("*** Error compiling %s:", currentProject));
        // Save the messages.
        currentErrors = result;
        // Write it to the console
        writeErrors(currentErrors);
        // Lint
        editorLint();
      } else {
        displayErrorMessage("Project could not be compiled.");
      }
    });
  }).fail(function () {
    // TODO: Better error handling.
    promise.reject(null);
    displayErrorMessage("Compilation failed because project could not be saved.");
  });

  return promise;
}

/**
 * Linter helper for projects.
 *
 * @return {Array of CodeMirror Linter messages} Result of linting the current file.
 */
function projectLinter() {
  var found = [];

  /** Look up the current errors for the current file. */
  for (var i = 0; i < currentErrors.length; i++) {
    var error = currentErrors[i][0];
    var file = currentErrors[i][1];
    var line = currentErrors[i][2];
    var column = currentErrors[i][3];
    var message = currentErrors[i][4];

    /** Correct for off by one errors. */
    if (line > 0) {
      line --;
    }

    if (file == currentFile || file == "final-link-result" ) {
      found.push({
        from: CodeMirror.Pos(line, column),
        to: CodeMirror.Pos(line),
        message: message,
        severity: error ? "error" : "warning"});
    }  
  }

  return found;
}

/**
 * Project runner.
 */
function projectRun() {
  var compile_promise = projectCompile();
  var promise = $.Deferred();

  // function which actually runs the project (without compiling)
  function run() {
    socket.runProject(currentProject, currentFile, promise);

    promise.done(function(pid) {
      consoleClear();
      if (consoleDebug()) {
        consoleWrite(sprintf("--- Running project '%s' [PID: %d] ---\n", currentProject, pid));
      } else {
        consoleWrite(sprintf("--- Running project '%s' ---\n", currentProject));
      }
      
      currentPID = pid;
    }).fail(function() {
      displayErrorMessage("Project could not be run.");
    });
  }

  if(currentFile.split('.').pop() == "rkt"){
    run();
    return;
  }

  // TODO: If current PID is set, kill it.  This _is_
  // a bit of a race condition, but the side effects
  // (in JavaScript) are negligible, and it shouldn't
  // happen that often anyways.
  // 
  // This can, and will happen whenever handles are reused.
  // Oh well. 

  /** We really ought not to run a project without compiling it. */
  compile_promise.done(run).fail(function () {
    promise.reject(null);
    displayErrorMessage("Project compilation failed.");
  });

  return promise;
}

/**
 * Project I/O handler
 */
function projectIOHandler(ignored, message) {
  if (message.type == "stdout" || message.type == "stderr") {
    consoleWriteRaw(message.message);
  } else if (message.type == "done") {
    if (consoleDebug()) {
      consoleWrite(sprintf("--- Terminated [PID: %d] with exit code %d ---\n", message.pid, message.status));
    } else {
      consoleWrite(sprintf("--- Terminated with exit code %d ---\n", message.status));
    }
    
    if (currentPID == message.pid) {
      currentPID = null;
    }
  }
}

/**
 * Project commit.
 * @param {String} description - Description to tag commit with.
 */
function projectCommit(description) {
  var save_promise = projectSave();
  var promise = $.Deferred();

  save_promise.done(function() {
    promise = socket.saveProject(currentProject, description);
    promise.fail(function() {
      displayErrorMessage("Commit failed because project could not be saved.");
    });
  }).fail(function() {
    promise.reject(null);
    displayErrorMessage("Project could not be committed.");
  });

  return promise;
}

/**
 * Project upload file form handler
 */
function projectUploadHandler() {
  /** Hide the modal. */
  $("#upload-file-dialog").modal("hide");
  /** Get the filename. */
  var filename = $("#file-to-upload").val().replace(/.*(\/|\\)/, '');
  /** Get the ticket. */
  var promise = socket.getUploadFileToken(currentProject, filename);
  promise.done(function (token) {
    var raw = JSON.stringify(token);
    var options = {
      target: null,
      dataType: null,
      error: function() {
        displayErrorMessage("File could not be successfully uploaded.");
      },
      success: function() {
        currentFiles[filename] = {};
        fileNavigationAddEntry(filename);
      },
      data: {token: raw},
      url: sprintf("https://%s:%s/upload", creds.host, creds.port)
    };
    /** Submit the form */
    $("#upload-file-form").ajaxSubmit(options);
  }).fail(function() {
    displayErrorMessage("Error retrieving file upload ticket.");
  });
  return false;
}

/**
 * Project Setup Function
 **/
function setupProjects() {
  /** Install the I/O handler. */
  socket.requests[-3].callback = projectIOHandler;
}

/**
 * Project input handler
 * @param {String} input Input to send to project.
 */
function projectInput(input) {
  // TODO: Error handling and PID
  var promise = socket.programInput(currentPID, input);
  promise.fail(function () {
    displayErrorMessage("Input was not successfully sent.");
  });
  return promise;
}

/**
 * Project download handler.
 */
function projectDownload() {
  return socket.getExportToken(currentProject).done(
      function(token) {
        /** Clear out any existing iframes. */
        $("#download-iframe").remove();

        /** Create a new one. */
        var raw = JSON.stringify(token);
        var frame =  $("<iframe>").attr("src",
                                        sprintf("https://%s:%s/export/%s.zip?token=%s",
                                          creds.host, creds.port,
                                          encodeURIComponent(currentProject),
                                          encodeURIComponent(raw)))
                                  .attr("id", "download-iframe");

        /** Errors will be shown below. */
        $("#download-project-body").append(frame);
        $("#download-project-dialog").modal("show");
      })
  .fail(function() {
    displayErrorMessage("Error retrieving project download token.");
  });
}

/* Handles the UI aspect of renaming files, and attaches a handler
  to make a websocket request when user presses enter or clicks away.
  Parameter ev is the click event that began the rename. */
function fileRename(ev) {
  var lnk = $(ev.target);
  var trash = lnk.children().last();
  lnk.unbind("click");
  var inp = $("<input type='text' class='form-control' value='"+lnk.text()+"' />");
  lnk.hide().parent().prepend(inp);
  inp.focus().select();

  var saveRename = function() {
    socket.renameFile(currentProject, lnk.text(), inp.val())
      .done(function() {
        var newname = inp.val();
        var oldname = lnk.text();
        lnk.text(newname);
        inp.remove();
        lnk.click(fileRename)
          .append(trash.click(fileNavigationTrashHandler(newname))).show();
        
        if(oldname != newname) {
          currentFile = newname;
          currentFiles[newname] = currentFiles[oldname];
          delete currentFiles[oldname];
        }
      })
      .fail(function() {
        displayErrorMessage("File could not be renamed.");
      });
  };

  inp.keydown(function(e) {
    if(e.which==13) saveRename();
  }).focusout(saveRename);
}

/**
 * Writes user settings to their homedir on backend
 */
function writeSettings(settings){
  socket.saveSettings(settings);
}

/**
 * Reads user settings from their homedir on backend (uses defaults if they
 * don't exist), updates the global settings variable, and applies the settings
 *
 * Parameters:
 *  succ, a callback which is called when settings are successfully refreshed
 *  fail, a callback which is called when settings fail to be refreshed
 */
// TODO: call this function after logging into seashell
function refreshSettings(succ, fail){
  // default settings
  var defaults = {
    font_size  : 10,
    edit_mode  : "standard",
    tab_width  : 4,
    use_space  : true,
    text_style : "default"
  };

  // function which applies the currently loaded settings to CodeMirror
  function applySettings(){
    $(".CodeMirror").css("font-size", settings["font_size"] + "pt");
    if(settings["edit_mode"] == "vim"){
        editor.setOption("vimMode", true);
    }else if(settings["edit_mode"] == "emacs"){
        editor.setOption("vimMode", false);
        editor.setOption("keyMap", "emacs");
    }else{
        editor.setOption("vimMode", false);
        editor.setOption("keyMap", "default");
    }
    editor.setOption("tabSize", settings["tab_width"]);
    editor.setOption("indentUnit", settings["tab_width"]);
    editor.setOption("theme", settings["text_style"]);
    // TODO: implement expandtab

    // change the options in the Settings menu to the new settings
    $("#editor_font").val(settings["font_size"]);
    $("#editor_mode").val(settings["edit_mode"]);
    $("#tab_width").val(settings["tab_width"]);
    $("#text_style").val(settings["text_style"]);
  }
    
    
  // read user settings from server, or use default if no settings exist
  var promise = socket.getSettings();
  promise.done(function (res){
    settings = defaults;
    if (res) {
      for(var key in res) {
        settings[key] = res[key];
      }
    }
    applySettings();
    if(succ) succ();
  }).fail(function (res){
    console.log("ERROR: Could not read settings from server.");
    if(fail) fail();
    return;
  });
}
