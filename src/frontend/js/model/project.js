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
var settings = null;

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

/*
 * Constructor for SeashellProject
 * Parameters:
 * name - full file path & name, eg. tests/small.in
 * callback - function to be called after project is created. Will be passed the
 *    the new SeashellProject as a parameter.
*/
function SeashellProject(name, callback) {
  this.name = name;
  this.files = null;
  this.currentFile = null;
  this.currentErrors = null;
  this.currentPID = null;
  var p = this;
  this.saveInterval = setInterval(function() { p.save(); }, 10000);
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

  var p = this;
/**
 * Opens a new project and sets the current project (without locking).
 * @param {String} project Name of project to open.
 */
  function projectOpenNoLock(name) {
    var promise = socket.listProject(name);

    /** Update the list of files. */
    promise.done(function(files) {
      p.files = [];
      p.currentErrors = [];
      saveTimeout = window.setTimeout(p.save, 10000);

      for (var i = 0; i < files.length; i++) {
        /** Lazily load the documents later. */
        p.placeFile(new SeashellFile(files[i]));
      }
      if(callback) callback(p);
    }).fail(function(){
      displayErrorMessage("Project "+name+" could not be opened.");
    });
  }

  /** Install the I/O handler. */
  socket.requests[-3].callback = this.IOHandler;
}

/*
 * Opens a project with the given name.
 * callback is called with the new SeashellProject instance once it is created.
*/
SeashellProject.open = function(name, callback) {
  SeashellProject.currentProject = new SeashellProject(name, callback);
};

SeashellProject.currentProject = null;

/*
 * Constructor for SeashellFile
 * Parameters:
 * name - the full file path & name from root of project
*/
function SeashellFile(name) {
  this.name = name.split("/");
  this.document = null;
  this.children = null;
}

/*
 * Returns the full file path and name for a SeashellFile
*/
SeashellFile.prototype.fullname = function() {
  return this.name.join("/");
};

/*
 * Creates a new file in the project
 * fname should be the full path from project root, ie. dir/fname.c
*/
SeashellProject.prototype.createFile = function(fname) {
  var p = this;
  if(this.exists(fname)) {
    displayErrorMessage("File "+fname+" already exists.");
    return null;
  }
  else {
    return socket.newFile(this.name, fname).done(function() {
      var nFile = new SeashellFile(fname);
      nFile.document = CodeMirror.Doc("/**\n * File: "+fname+"\n * Enter a description of this file.\n*/\n");
      p.placeFile(nFile);
      p.openFile(nFile);
    }).fail(function() {
      displayErrorMessage("Error creating the file "+fname+".");
    });
  }
};

/*
 * Places a file in the correct place in a file
 * file - a SeashellFile instance
 * removeFirst - if true, first searches files and removes the given
 *    file if it is already in the project.
*/
SeashellProject.prototype.placeFile = function(file, removeFirst) {
 function rmv(aof) {
    for(f in aof) {
      if(f.children) {
        f.children = rmv(f.children);
      }
      else if(f === file) {
        return aof.splice(aof.indexOf(f), 1);
      }
    }
    return aof;
  }

  function plc(aod, aof) {
    if(aod.length > 1) {
      for(f in aof) {
        if(f.children && f.name[f.name.length-1] == aod[0]) {
          f.children = plc(aod.slice(1), f.children);
        }
      }
    }
    else {
      aof.push(file);
    }
    return aof;
  }

  if(removeFirst) {
    this.files = rmv(this.files);
  }
  if(file) {
    this.files = plc(file.name, this.files);
  }
};

/*
 * Opens the given file in the project
 * file - a SeashellFile instance
*/
SeashellProject.prototype.openFile = function(file) {
  if(file.children) return null;
  this.currentFile = file;
  if(file.document) return null;
  else {
    return socket.readFile(this.name, file.name.join("/"))
      .done(function(contents) {
        file.document = CodeMirror.Doc(contents, "text/x-csrc");
      })
      .fail(function() {
        displayErrorMessage("Error reading file "+file.name+".");
      });
  }
};

 /** 
 * Creates and opens a project.
 * @param {String} project Name of project to create.
 */
SeashellProject.createProject = function(name) {
  var promise = socket.newProject(name);

  /** Open it. */
  promise.done(function() {
    SeashellProject.currentProject = new SeashellProject(name);
  }).fail(function() {
    displayErrorMessage("Project "+name+" could not be created.");
  });
};

/*
 * Saves all files in the project
 */
SeashellProject.prototype.save = function() {
  var promises = [];
  var p = this;
  function save_arr(aof) {
    for(file in aof) {
      if(file.children) save_arr(file.children);
      else promises.push(file.save(this.name));
    }
  }
  return $.when.apply(null, promises)
    .done(function() {
      window.clearTimeout(p.saveTimeout);
      p.saveTimeout = window.setTimeout(p.save, 10000);
    });
};

/*
 * Saves the file.
 * pname - name of the file's project
 */
SeashellFile.prototype.save = function(pname) {
  return socket.writeFile(pname, this.fullname(), this.document)
    .fail(function() {
      displayErrorMessage("File "+this.fullname()+" could not be saved.");
    });
};

/*
 * Requests the list of all user's projects.
*/
SeashellProject.getListOfProjects = function() {
  return socket.getProjects()
    .fail(function() {
      displayErrorMessage("Could not fetch list of projects.");
    });
};

/*
 * Closes the project.
 * save - If true, saves project before closing
*/
SeashellProject.prototype.closeProject = function(save) {
  if(this === SeashellProject.currentProject) {
    socket.unlockProject(this.name);
    if(save) this.save();
    window.clearTimeout(this.saveTimeout);
    SeashellProject.currentProject = null;
    delete this;
  }
};

/*
 * Closes the file.
 * save - If true, saves the file before closing
 */
SeashellProject.prototype.closeFile = function(save) {
  if(this.currentFile) {
    if(save) this.currentFile.save();
    this.currentFile = null;
  }
};

/*
 * Deletes a file in the project.
 * file - a SeashellFile instance that is in the project
*/
SeashellProject.prototype.deleteFile = function(file) {
  if(file == this.currentFile) this.closeFile(false);

  function rmv(aof) {
    for(var f=0; f < aof.length; f++) {
      if(aof[f] == file) {
        aof.splice(aof.indexOf(aof[f]), 1);
        return aof;
      }
      else if(aof[f].children) {
        aof[f].children = rmv(aof[f].children);
      }
    }
    return aof;
  }
  var p = this;
  return socket.deleteFile(this.name, file.fullname())
    .done(function() {
      p.files = rmv(p.files);
    })
    .fail(function() {
      displayErrorMessage("File "+file.fullname()+" could not be deleted.");
    });
};

/*
 * Deletes the project.
*/
SeashellProject.prototype.deleteProject = function() {
  var nm = this.name;
  return socket.deleteProject(nm)
    .done(function() {
      this.projectClose(false);
    })
    .fail(function() {
      displayErrorMessage("Project "+nm+" could not be deleted.");
    });
};

/*
 * Compiles the project.
 */
SeashellProject.prototype.compile = function() {
  var save_promise = this.save();
  var promise = $.Deferred();
  var p = this;
  save_promise.done(function () {
    socket.compileProject(p.name, promise);

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
      this.currentErrors = messages;
      // Write it to the console
      writeErrors(this.currentErrors);
      // Lint
      editorLint();
    }).fail(function(result) {
      if (Array.isArray(result)) {
        // Log
        consoleWrite(sprintf("*** Error compiling %s:", this.name));
        // Save the messages.
        this.currentErrors = result;
        // Write it to the console
        writeErrors(this.currentErrors);
        // Lint
        editorLint();
      } else {
        displayErrorMessage("Project could not be compiled.");
      }
    });
  }).fail(function () {
    promise.reject(null);
    displayErrorMessage("Compilation failed because project could not be saved.");
  });

  return promise;
};

/**
 * Linter helper for projects.
 *
 * @return {Array of CodeMirror Linter messages} Result of linting the current file.
 */
SeashellProject.linter = function() {
  var found = [];
  if(SeashellProject.currentProject) {
    /** Look up the current errors for the current file. */
    for (var i = 0; i < SeashellProject.currentProject.currentErrors.length; i++) {
      var error = SeashellProject.currentProject.currentErrors[i][0];
      var file = SeashellProject.currentProject.currentErrors[i][1];
      var line = SeashellProject.currentProject.currentErrors[i][2];
      var column = SeashellProject.currentProject.currentErrors[i][3];
      var message = SeashellProject.currentProject.currentErrors[i][4];

      /** Correct for off by one errors. */
     if (line > 0) {
        line --;
      }

     if (file == this.currentFile || file == "final-link-result" ) {
        found.push({
          from: CodeMirror.Pos(line, column),
          to: CodeMirror.Pos(line),
          message: message,
          severity: error ? "error" : "warning"});
      }  
    }
  }

  return found;
};

/*
 * Runs the currently open project.
 */
SeashellProject.run = function() {
  if(SeashellProject.currentProject) {
    return SeashellProject.currentProject.run();
  }
  return null;
};

/*
 * Runs the project.
 */
SeashellProject.prototype.run = function(withTests, tests) {
  // TODO: run with tests
  if(withTests) return null;

  var compile_promise = this.currentFile.name.join('/').split('.').pop() == "rkt" ? null : this.compile();
  var promise = $.Deferred();

  // function which actually runs the project (without compiling)
  function run() {
    socket.runProject(this.name, this.currentFile.name.join('/'), promise);

    promise.done(function(pid) {
      consoleClear();
      if (consoleDebug()) {
        consoleWrite(sprintf("--- Running project '%s' [PID: %d] ---\n", currentProject, pid));
      } else {
        consoleWrite(sprintf("--- Running project '%s' ---\n", currentProject));
      }
      
      this.currentPID = pid;
    }).fail(function() {
      displayErrorMessage("Project could not be run.");
    });
  }

  if(this.currentFile.name.join('/').split('.').pop() == "rkt"){
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
};

/*
 * Callback function that handles program input & output activity
*/
SeashellProject.prototype.IOHandler = function(ignored, message) {
   if (message.type == "stdout" || message.type == "stderr") {
    consoleWriteRaw(message.message);
  } else if (message.type == "done") {
    if (consoleDebug()) {
      consoleWrite(sprintf("--- Terminated [PID: %d] with exit code %d ---\n", message.pid, message.status));
    } else {
      consoleWrite(sprintf("--- Terminated with exit code %d ---\n", message.status));
    }
    
    if (this.currentPID == message.pid) {
      this.currentPID = null;
    }
  }
};

/*
 * Should commit the project, currently just saves.
 */
SeashellProject.prototype.commit = function(description) {
  // TODO: actually commit
  return this.save();
};

/*
 * Requests an upload token for the project.
 * filename - the filename to be uploaded to
 */
SeashellProject.prototype.getUploadToken = function(filename) {
  return socket.getUploadFileToken(this.name, filename)
    .fail(function() {
      displayErrorMessage("Failed to get upload token.");
    });
};

/*
 * Function called on successful file upload.
 * filename - the filename that was uploaded
 */
SeashellProject.prototype.onUploadSuccess = function(filename) {
  this.placeFile(new SeashellFile(filename));
};

/*
 * Sends input to the currently running project.
 * input - the actual input to be sent
 */
SeashellProject.prototype.input = function(input) {
  return socket.programInput(this.currentPID, input)
    .fail(function() {
      displayErrorMessage("Input was not successfully sent.");
    });
};

/*
 * Requests a download token for the project.
 */
SeashellProject.prototype.getDownloadToken = function() {
  return socket.getExportToken(this.name)
    .fail(function() {
      displayErrorMessage("Could not get project download token.");
    });
};

/*
 * Renames a file in the project.
 * file - SeashellFile instance to rename
 * name - new name (full path from project root) of the file
 */
SeashellProject.prototype.renameFile = function(file, name) {
  return socket.renameFile(this.name, file.fullname(), name)
    .done(function() {
      file.name = name.split("/");
      this.placeFile(file, true);
    })
    .fail(function() {
      displayErrorMessage("File could not be renamed.");
    });
};

/*
 * Returns an object formatted for use in the JSTree plugin
 */
SeashellProject.prototype.JSTreeData = function() {
  var nodes = [];
  function JSTreeHelper(arr, res) {
    for(var i=0; i < arr.length; i++) {
      var item = {text: arr[i].name[arr[i].name.length-1]};
      item.path = arr[i].fullname();
      if(arr[i].children) {
        var n = [];
        JSTreeHelper(arr[i].children, n);
        item.children = n;
        item.icon = "glyphicon glyphicon-folder-closed";
      }
      else {
        item.icon = "glyphicon glyphicon-file";
      }
      res.push(item);
    }
  }
  if(this.files) {
    JSTreeHelper(this.files, nodes);
  }
  console.log(nodes);
  return nodes;
};

/* finds a SeashellFile in the project given its path as a string
*/
SeashellProject.prototype.getFileFromPath = function(path) {
  function find(array, path) {
    for(var i=0; i < array.length; i++) {
      if(array[i].name[array[i].name.length-1] == path[0]) {
        if(path.length==1) {
          return array[i];
        }
        else {
          return find(array[i].children, path.slice(1));
        }
      }
    }
    return false;
  }
  return find(this.files, path.split('/'));
};

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

/* predicate to determine if given filename exists in the project 
*/
SeashellProject.prototype.exists = function(fname) {
  function check(aof) {
    for(var f=0; f < aof.length; f++) {
      if(aof[f].fullname() == fname)
        return true;
      if(aof[f].children) {
        if(check(aof[f].children))
          return true;
      }
    }
    return false;
  }
}
