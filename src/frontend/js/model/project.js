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

  return $.when.apply(null, promises);
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
 * Closes the project.
 *
 * @param {Boolean} save Save project on close or not.
 */
function projectClose(save) {
  // TODO: Unlock project.
  if (save)
    projectSave();
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
    currentFiles[name].tag.addClass("active");
    /** Load the file. */
    currentFile = name;
  }

  if (! ("document" in currentFiles[name])) {
    socket.readFile(currentProject, name).done(function (contents) {
      // TODO: Custom file types that are not C source files.
      // Consult lib/codemirror/mode and stuff.
      currentFiles[name].document = new CodeMirror.Doc(contents, "text/x-csrc");
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
    // TODO: Proper new file call.
    saveFile(name).done(function () {;
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
          $("<span>").addClass("pull-right").addClass("glyphicon").addClass("glyphicon-trash").on("click", function() {deleteFileDialog(file);}))
        .on("click",
        function (event) {
          /** Only handle click events for the A tag itself, not for the span inside it. */
          if (event.target == this)
            fileOpen(file);
        }));
  // Add the tag.
  $("#file-navigator").append(tag);
  // Save the tag for easy access.
  currentFiles[file].tag = tag; 
}

/**
 * Opens and sets the current project.
 * @param {String} project Name of project to open.
 */
function projectOpen(name) {
  var promise = socket.listProject(name);

  // TODO: Lock Project
  // This (probably, should) not be a hard lock,
  // but a soft lock that will warn if the project is possibly
  // open somewhere else.

  /** Update the list of files. */
  promise.done(function(files) {
    projectClose(true);
    currentFiles = {};
    currentProject = name;
    currentErrors = [];

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

  // TODO: If current PID is set, kill it.  This _is_
  // a bit of a race condition, but the side effects
  // (in JavaScript) are negligible, and it shouldn't
  // happen that often anyways.
  // 
  // This can, and will happen whenever handles are reused.
  // Oh well. 

  /** We really ought not to run a project without compiling it. */
  compile_promise.done(function () {
    socket.runProject(currentProject, promise);

    promise.done(function(pid) {
      consoleWrite(sprintf("--- Launching project %s - PID %d.\n", currentProject, pid));
      currentPID = pid;
    }).fail(function() {
      displayErrorMessage("Project could not be run.");
    });
  }).fail(function () {
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
    consoleWrite(sprintf("--- PID %d exited with status %d.", message.pid, message.status));
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
