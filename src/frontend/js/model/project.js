"use strict";
/**
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
    // TODO: Error handling.
  });
}

/**
 * Saves everything in the current project.
 */
function projectSave() {
  for (var file in currentFiles) {
    if ("document" in currentFiles[file]) {
      saveFile(file);
    }
  }
}

/**
 * Saves a file.
 * @param {String} file Name of file to save. 
 */
function saveFile(file) {
  socket.writeFile(currentProject, file, currentFiles[file].document.getValue()).fail(function() {
    // TODO: Error handling.
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
  }).fail(function() {
    // TODO: Error handling.
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
     // TODO: Error handling.
    }); 
  } else {
    rest( );
  }
}

/**
 * Creates a file.
 * @param {String} file Name of file to create.
 */
function fileNew(name) {
  /** (Lazily) create the file. */
  if (name in currentFiles) {
    // TODO: Error handling.  This should never happen,
    // as a project should only be open in one Seashell instance
    // at any time.  Though that still has to be handled.
  } else {
    // TODO: Custom file types.
    currentFiles[name] = {"document": CodeMirror.Doc("/**\n * File: " + name + "\n * Enter a description of this file.\n*/", "text/x-csrc")};
    saveFile(name);
    fileNavigationAddEntry(name);
    fileOpen(name);
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
  }).fail(function(){
    // TODO: error handling.
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
    // TODO: error handling.
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
    // TODO: error handling.
  });
}

/**
 * Compiles the current project.
 */
function projectCompile() {
  projectSave();
  var promise = socket.compileProgram(currentProject);

  /** Deal with it. */
  promise.done(function(messages) {
    // Save the messages.
    currentErrors = messages;
    editorLint();
  }).fail(function(result) {
    if (Array.isArray(result)) {
      currentErrors = result;
      editorLint();
    } else {
      // TODO: error handling.
    }
  });
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

function projectRun() {}
