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

SeashellProject.new = function(name) {
  return socket.newProject(name)
    .fail(function() {
      displayErrorMessage("Project "+name+" could not be created.");
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
      p.saveInterval = setInterval(function() { p.save(); }, 10000);

      for (var i = 0; i < files.length; i++) {
        /** Lazily load the documents later. */
        p.placeFile(new SeashellFile(files[i][0], files[i][1]));
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
  if(SeashellProject.currentProject)
    SeashellProject.currentProject.close();
  SeashellProject.currentProject = new SeashellProject(name, callback);
};

SeashellProject.currentProject = null;

/*
 * Constructor for SeashellFile
 * Parameters:
 * name - the full file path & name from root of project
 * is_dir - boolean, true if this is a directory. Default is false.
*/
function SeashellFile(name, is_dir) {
  this.name = name.split("/");
  this.document = null;
  this.children = is_dir ? [] : null;
  this.is_dir = is_dir ? true : false;
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
      var ext = fname.split(".").pop();
      var def = "";
      if(ext=="c"||ext=="h") {
        def = "/**\n * File: "+fname+"\n * Enter a description of this file.\n*/\n";
      }
      else if(ext=="rkt") {
        def = "#lang racket\n;; File: "+fname+"\n;; Enter a description of this file.\n";
      }
      nFile.document = CodeMirror.Doc(def, "text/x-csrc");
      p.placeFile(nFile);
      p.openFile(nFile);
    }).fail(function() {
      displayErrorMessage("Error creating the file "+fname+".");
    });
  }
};

/*
 * Creates a new directory in the project.
 * dname should be full path from project root.
*/
SeashellProject.prototype.createDirectory = function(dname) {
  var p = this;
  if(this.exists(dname)) {
    displayErrorMessage("Directory "+dname+" already exists.");
    return null;
  }
  return socket.newDirectory(this.name, dname).done(function() {
    var dirObj = new SeashellFile(dname, true);
    p.placeFile(dirObj);
  }).fail(function() {
    displayErrorMessage("Error creating the directory "+dname+".");
  });
}

/*
 * Places a file in the correct place in a project
 * file - a SeashellFile instance
 * removeFirst - if true, first searches files and removes the given
 *    file if it is already in the project.
*/
SeashellProject.prototype.placeFile = function(file, removeFirst) {
 function rmv(aof) {
    for(var f in aof) {
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
      for(var i=0; i < aof.length; i++) {
        if(aof[i].is_dir && aof[i].name[aof[i].name.length-1] == aod[0]) {
          aof[i].children = plc(aod.slice(1), aof[i].children);
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

/*
 * Saves all files in the project
 */
SeashellProject.prototype.save = function() {
  var promises = [];
  var p = this;
  function save_arr(aof) {
    for(var f=0; f < aof.length; f++) {
      if(aof[f].children) save_arr(aof[f].children);
      else promises.push(aof[f].save(p.name));
    }
  }
  save_arr(this.files);
  return $.when.apply(null, promises)
    .fail(function() {
      displayErrorMessage("Project failed to save.");
    });
};

/*
 * Saves the file.
 * pname - name of the file's project
 */
SeashellFile.prototype.save = function(pname) {
  if(this.document) {
    return socket.writeFile(pname, this.fullname(), this.document.getValue())
      .fail(function() {
        displayErrorMessage("File "+this.fullname()+" could not be saved.");
      });
  }
  return $.Deferred().resolve().promise();
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
SeashellProject.prototype.close = function(save) {
  if(this === SeashellProject.currentProject) {
    var proms = [];
    proms.push(socket.unlockProject(this.name));
    if(save) proms.push(this.save());
    window.clearInterval(this.saveInterval);
    SeashellProject.currentProject = null;
    delete this;
    return $.when.apply(proms)
      .fail(function() {
        displayErrorMessage("Project could not be closed.");
      });
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
 * Deletes a file OR directory in the project.
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
  if(file.is_dir) {
    return socket.deleteDirectory(this.name, file.fullname())
      .done(function() {
        p.files = rmv(p.files);
      })
      .fail(function() {
        displayErrorMessage("Directory "+file.fullname()+" could not be deleted.");
      });
  }
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
SeashellProject.prototype.delete = function(callback) {
  var p = this;

  this.close().done(function() {
    socket.deleteProject(p.name)
      .done(function() {
        if(callback)
          callback();
      })
      .fail(function() {
        displayErrorMessage("Project "+p.name+" could not be deleted.");
      });
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
    socket.compileProject(p.name, p.currentFile.fullname(), promise);

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
      p.currentErrors = messages;
      // Write it to the console
      writeErrors(p.currentErrors);
      // Lint
      editorLint();
    }).fail(function(result) {
      if (Array.isArray(result)) {
        // Log
        consoleWrite(sprintf("*** Error compiling %s:", p.name));
        // Save the messages.
        p.currentErrors = result;
        // Write it to the console
        writeErrors(p.currentErrors);
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

/**
 * Sends EOF to the currently running project.
 */
function sendEOF() {
    if(currentPID) socket.sendEOF(currentPID);
}

/**
 * Project runner.
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
  var p = this;

  // function which actually runs the project (without compiling)
  function run() {
    socket.runProject(p.name, p.currentFile.name.join('/'), promise);

    promise.done(function(pid) {
      consoleClear();
      if (consoleDebug()) {
        consoleWrite(sprintf("--- Running project '%s' [PID: %d] ---\n", p.name, pid));
      } else {
        consoleWrite(sprintf("--- Running project '%s' ---\n", p.name));
      }
      
      p.currentPID = pid;
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
  var p = this;
  return socket.renameFile(this.name, file.fullname(), name)
    .done(function() {
      file.name = name.split("/");
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
      if(arr[i].is_dir) {
        var n = [];
        if(arr[i].children)
          JSTreeHelper(arr[i].children, n);
        item.children = n;
        item.icon = 'glyphicon glyphicon-folder-open';
      }
      else {
        item.icon = 'glyphicon glyphicon-file';
      }
      res.push(item);
    }
  }
  if(this.files) {
    JSTreeHelper(this.files, nodes);
  }
  return nodes;
};

/* finds a SeashellFile in the project given its path as a string
*/
SeashellProject.prototype.getFileFromPath = function(path) {
  function find(array, p) {
    for(var i=0; i < array.length; i++) {
      if(array[i].name[array[i].name.length-1] == p[0]) {
        if(p.length==1) {
          return array[i];
        }
        else {
          return find(array[i].children, p.slice(1));
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
