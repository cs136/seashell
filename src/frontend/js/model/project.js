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

function SeashellProject(name) {
  this.name = name;
  this.files = null;
  this.currentFile = null;
  this.currentErrors = null;
  this.currentPID = null;
  this.saveTimeout = null;

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

/**
 * Opens a new project and sets the current project (without locking).
 * @param {String} project Name of project to open.
 */
  function projectOpenNoLock(name) {
    var promise = socket.listProject(name);

    /** Update the list of files. */
    promise.done(function(files) {
      this.files = [];
      this.currentErrors = [];
      saveTimeout = window.setTimeout(projectSave, 10000);

      for (var i = 0; i < files.length; i++) {
        /** Lazily load the documents later. */
        this.files[files[i]] = new SeashellFile(files[i]);
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

  /** Install the I/O handler. */
  socket.requests[-3].callback = this.IOHandler;
}

SeashellProject.currentProject = null;

function SeashellFile(name) {
  this.name = name.split("/");
  this.document = null;
  this.children = null;
}

SeashellFile.prototype.fullname = function() {
  return this.name.join("/");
}

/* fname should be the full path from project root, ie. dir/fname.c
*/
SeashellProject.prototype.createFile = function(fname) {
  return socket.newFile(this.name, fname).done(function() {
    var nFile = new SeashellFile(fname);
    this.files.push(nFile);
    this.openFile(nFile);
  }).fail(function() {
    displayErrorMessage("Error creating the file "+fname+".");
  });
}

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
}

SeashellProject.prototype.openFile = function(file) {
  if(file.children || file.document) return null;
  else {
    return socket.readFile(this.name, this.resolvePath(file))
      .done(function(contents) {
        file.document = contents;
      })
      .fail(function() {
        displayErrorMessage("Error reading file "+file.name+".");
      });
  }
}



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
}

SeashellProject.prototype.saveProject = function() {
  var promises = [];
  function save_arr(aof) {
    for(file in aof) {
      if(file.children) save_arr(file.children);
      else promises.push(file.save(this.name));
    }
  }
  $.when.apply(null, promises)
    .done(function() {
      window.clearTimeout(this.saveTimeout);
      this.saveTimeout = window.setTimeout(this.saveProject, 10000);
    });
}

SeashellFile.prototype.save = function(pname) {
  return socket.writeFile(pname, this.fullname(), this.document)
    .fail(function() {
      displayErrorMessage("File "+this.fullname()+" could not be saved.");
    });
}

SeashellProject.getListOfProjects = function() {
  return socket.getProjects()
    .fail(function() {
      displayErrorMessage("Could not fetch list of projects.");
    });
}

SeashellProject.prototype.closeProject = function(save) {
  if(this === SeashellProject.currentProject) {
    socket.unlockProject(this.name);
    if(save) this.saveProject();
    window.clearTimeout(this.saveTimeout);
    SeashellProject.currentProject = null;
    delete this;
  }
}

SeashellProject.prototype.closeFile = function(save) {
  if(this.currentFile) {
    if(save) this.currentFile.save();
    this.currentFile = null;
  }
}

SeashellProject.prototype.deleteFile = function(file) {
  if(file === this.currentFile) this.closeFile(false);

  function rmv(aof) {
    for(f in aof) {
      if(f.children) {
        f.children = rmv(f.children);
      }
      else if(f == file) {
        return aof.splice(aof.indexOf(f), 1);
      }
    }
    return aof;
  }

  return socket.deleteFile(this.name, file.fullname())
    .done(function() {
      rmv(this.files);
    })
    .fail(function() {
      displayErrorMessage("File "+file.fullname()+" could not be deleted.");
    });
}

SeashellProject.prototype.deleteProject = function() {
  var nm = this.name;
  return socket.deleteProject(nm)
    .done(function() {
      this.projectClose(false);
    })
    .fail(function() {
      displayErrorMessage("Project "+nm+" could not be deleted.");
    });
}

SeashellProject.prototype.compile() {
  var save_promise = this.saveProject();
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

SeashellProject.prototype.run(withTests, tests) {
  // TODO: run with tests
  if(withTests) return null;

  var compile_promise = this.compile();
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
  }).fail(function () {
    promise.reject(null);
    displayErrorMessage("Project compilation failed.");
  });

  return promise;
}

SeashellProject.prototype.IOHandler = function(ignored, message) {
   if (message.type == "stdout" || message.type == "stderr") {
    consoleWriteRaw(message.message);
  } else if (message.type == "done") {
    if (consoleDebug()) {
      consoleWrite(sprintf("--- Terminated [PID: %d] with exit code %d ---\n", message.pid, message.status));
    } else {
      consoleWrite("--- Terminated with exit code %d ---\n", message.status);
    }
    
    if (this.currentPID == message.pid) {
      this.currentPID = null;
    }
  }
}

SeashellProject.prototype.commit = function(description) {
  // TODO: actually commit
  return this.save();
}

SeashellProject.prototype.getUploadToken = function(filename) {
  return socket.getUploadFileToken(this.name, filename)
    .fail(function() {
      displayErrorMessage("Failed to get upload token.");
    });
}

SeashellProject.prototype.onUploadSuccess = function(filename) {
  this.files.push(new SeashellFile(filename));
}

SeashellProject.prototype.input = function(input) {
  return socket.programInput(this.currentPID, input)
    .fail(function() {
      displayErrorMessage("Input was not successfully sent.");
    });
}

SeashellProject.prototype.getDownloadToken = function() {
  return socket.getExportToken(this.name)
    .fail(function() {
      displayErrorMessage("Could not get project download token.");
    });
}

SeashellProject.prototype.renameFile = function(file, name) {
  return socket.renameFile(this.name, file.fullname(), name)
    .done(function() {
      file.name = name.split("/");
      this.placeFile(file, true);
    })
    .fail(function() {
      displayErrorMessage("File could not be renamed.");
    });
}
