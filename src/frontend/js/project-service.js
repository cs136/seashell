/**
 * Angular bindings for Seashell projects.
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
 * along with self program.  If not, see <http://www.gnu.org/licenses/>.
 */
angular.module('seashell-projects', ['seashell-websocket', 'marmoset-bindings'])
  /**
   * Project [factory] service.
   * Provides functions to list/load/open/create new SeashellProject
   *  instances.
   */ 
  .service('projects', ['$rootScope', '$q', 'socket', 'marmoset',
    function($scope, $q, ws, marmoset) {
      "use strict";
      var self = this;
      var list_url = "https://www.student.cs.uwaterloo.ca/~cs136/cgi-bin/skeleton_list.cgi";
      var skel_template = "file:///u/cs136/public_html/assignment_skeletons/%s";
     
      var SeashellProject = (function () { 
        /**
         * SeashellProject.
         *
         * @constructor Constructor for a new SeashellProject.
         *  Project is not ready until .init() is called, unless
         *  read_only is true.
         */
        function SeashellProject (name, read_only) {
          var self = this;

          self.name = name;
          self.read_only = read_only;
        }

        function SeashellFile(project, name, is_dir, last_saved) {
          var self = this;
          self.name = name.split("/");
          self.project = project;
          self.children = is_dir ? null : [];
          self.is_dir = is_dir ? true : false;
          self.last_saved = last_saved ? last_saved : Date.now();
          self.unsaved = false;
        }

        SeashellFile.prototype.fullname = function() {
          var self = this;
          return self.name.join("/");
        }

        SeashellFile.prototype.save = function() {
          var self = this;
          if(self.unsaved !== false) {
            var thisTimer = f.unsaved;
            //TODO: Decide how to adjust this to account for single document being loaded model
            //   after switching to Angular frontend
            //   - most likely: save before switching files.
            return $q.when(ws.socket.writeFile(self.project.name, self.fullname(), self.document))
              .then(function() {
                self.last_saved = Date.now();
                // only set as unsaved if no modification has been made
                if(self.unsaved == thisTimer) self.unsaved = false;
              });
          }
          return $q.resolve();
        }

        SeashellFile.prototype.isUnsaved = function() {
          var self = this;
          if(self.is_dir)
            return _.foldl(self.children, function(b, f) {
              return f.isUnsaved() || b;
            }, false);
          return self.unsaved !== false;
        }

        SeashellFile.prototype.lastSavedString = function() {
          var self = this;
          var d = new Date(self.last_saved);
          return d.getFullYear() + "-" + (d.getMonth()+1) + "-" + d.getDate() + " " + d.getHours()
            + ":" + d.getMinutes() + ":" + d.getSeconds();
        }

        SeashellFile.prototype.ext = function() {
          var self = this;
          return self.name[self.name.length-1].split(".").pop();
        }

        /**
         * SeashellProject.init()
         *
         * Initializes a project.  A read-only project is already initialized.
         * @param force-lock - Forcibly lock this project?
         * @returns {Angular.$q} Deferred that resolves when this is done,
         *  or an error:
         *    "locked" - Project is locked.
         *    other    - Error message.
         */
        SeashellProject.prototype.init = function(force_lock) {
          var self = this;
          if (!force_lock) {
            return $q.when(ws.socket.lockProject(self.name)).then(function () {return this;});
          } else {
            return $q.when(ws.socket.forceLockProject(self.name)).then(function () {return this;});
          }
        };

        /** SeashellProject.questions()
         *
         * Lists all questions.
         * @returns {Angular.$q -> [string]} Promise that resolves to the list of questions.
         */
        SeashellProject.prototype.questions = function() {
          var self = this;
          return $q.when(ws.socket.listProject(self.name))
            .then(function (files) {
              return _.map(_.filter(_.map(files, function (file)
                      {return [file[0].split("/"), file[1]];}),
                      function (file) {
                        return file[0].length == 1 && file[0] !== "common" && file[1];
                      }),
                      function (file) {
                        return file[0][0];
                      });
            });
        };

        /**
         * SeashellProject.filesFor(question)
         *
         * Lists all files present in a question.
         */
        SeashellProject.prototype.filesFor = function(question) {
          var self = this;
          return $q.when(ws.socket.listProject(self.name))
            .then(function (files) {
              files = _.map(files, function (file) {return [file[0].split("/"), file[1], file[2]];});
              var common = _.filter(files, function (file) {return file[0][0] === "common" && !file[1];});
              var question_files = _.filter(files, function (file) {
                    return file[0][0] === question && !file[1] &&
                      (file[0].length == 1 ||
                       (file[0].length >= 2 && file[0][1] !== "tests"));
                  });
              var tests = _.filter(files, function (file) {
                return (file[0].length >= 2 && file[0][1] === "tests" && !file[1]);
              });

              return {common: common, question: question_files, tests: tests};
            });
        };

        /**
         * SeashellProject.createFile(fname)
         * 
         * Creates a new file in the project with the given name.
         */
        SeashellProject.prototype.createFile = function(fname) {
          var self = this;
          if(self.exists(fname)) {
            return $q.reject("A file with that name already exists.");
          }
          else {
            var dir = fname.split("/");
            dir.pop();
            return $q.when(ws.socket.createDirectory(dir.join("/"))
              .then(function() {
                return $q.when(ws.socket.newFile(self.name, fname))
                  .then(function() {
                    var nFile = new SeashellFile(self, fname);
                    var ext = fname.split(".").pop();
                    var def = "\n";
                    var doc_type = "text/plain";
                    if(ext=="c"||ext=="h") {
                      def = "/**\n * File: "+fname+"\n * Enter a description of this file.\n */\n";
                      doc_type = "text/x-csrc";
                    }
                    else if(ext=="rkt") {
                      def = "#lang racket\n;;  File: "+fname+"\n;; Enter a description of this file.\n";
                      doc_type = "text/x-scheme";
                    }
                    self.placeFile(nFile);
                    self.openFile(nFile);
                  })
                  .catch(function() {
                    return $q.reject("Could not create new file.");
                  });
                })
                .catch(function() {
                  return $q.reject("Could not create new directory for file.");
                });
            }
        };

        /**
         * SeashellProject.createDirectory(dname)
         *
         * Creates a new directory in the project at the given path.
         */
        SeashellProject.prototype.createDirectory = function(dname) {
          var self = this;
          if(self.exists(dname))
            return $q.resolve();
          return $q.when(ws.socket.newDirectory(self.name, dname)
            .then(function() {
              var dirObj = new SeashellFile(self, dname, true);
              self.placeFile(dirObj);
            });
        };

        /**
         * SeashellProject.placeFile(file, removeFirst)
         *
         * Places a SeashellFile (file param) in the internal directory
         *  structure of the project. If removeFirst is true, the file
         *  will be removed from the structure and then replaced.
         */
        SeashellProject.prototype.placeFile = function(file, removeFirst) {
          function rmv(aof) {
            for(var i=0; i<aof.length; i++) {
              if(aof[i].children)
                aof[i].children = rmv(f.children);
              else if(aof[i] === file)
                return aof.splice(i, 1);
            }
            return aof;
          }
          function plc(aod, aof) {
            if(aod.length > 1) {
              for(var i=0; i<aof.length; i++) {
                if(aof[i].is_dir && aof[i].name[aof[i].name.length-1] == aod[0])
                  aof[i].children = plc(aod.slice(1), aof[i].children);
              }
            }
            else
              aof.push(file);
            return aof;
          }
          if(removeFirst)
            self.files = rmv(self.files);
          if(file)
            self.files = plc(file.name, self.files);
        };

        /**
         * SeashellProject.openFilePath(path)
         *
         * Opens the file located at the given path, relative to the
         *  project root.
         */
        SeashellProject.prototype.openFilePath = function(path) {
          var self = this;
          var file = self.getFileFromPath(path);
          return self.openFile(file);
        }

        SeashellProject.prototype.openFile = function(file) {
          var self = this;
          if(file.is_dir)
            return $q.reject("Cannot open a directory in editor.");
          self.currentFile = file;
          return $q.when(ws.socket.readFile(self.name, file.name.join("/")))
            .then(function(contents) {
              var mime = "text/plain";
              var ext = file.ext();
              if(ext == "c"||ext == "h")
                mime = "text/x-csrc";
              else if(ext == "rkt")
                mime = "text/x-scheme";
              // TODO: Actually hook this up to CodeMirror
            });
        };

        /**
         * SeashellProject.save()
         *
         * Saves all unsaved files in the project.
         */
        SeashellProject.prototype.save = function() {
          var self = this;
          var proms = [];
          function save_arr(aof) {
            for(var f=0; f < aof.length; f++) {
              if(aof[f].is_dir) save_arr(aof[f].children);
              else proms.push(aof[f].save());
            }
          }
          save_arr(self.files);
          return $q.when($.when.apply(null, proms));
        };

        /**
         * SeashellProject.isUnsaved()
         *
         * Predicate, returns true if there exists an unsaved file
         *  in the project.
         */
        SeashellProject.prototype.isUnsaved = function() {
          var self = this;
          return _.foldl(self.files, function(b, f) {
            return f.isUnsaved() || b;
          }, false);
        };

        /**
         * SeashellProject.close(save)
         *
         * Closes the project. If the save param is true, saves the project
         *  before closing.
         */
        SeashellProject.prototype.close = function(save) {
          var self = this;
          var proms = [];
          proms.push(ws.socket.unlockProject(self.name));
          if(save) proms.push(self.save());
          $window.clearInterval(self.saveInterval);
          SeashellProject.currentProject = null;
          delete self;
          return $q.when($.when.apply(proms));
        };

        /**
         * SeashellProject.closeFile(save)
         *
         * Closes the currently open file. If the save param is true, saves
         *  the file before closing.
         */
        SeashellProject.prototype.closeFile = function(save) {
          var self = this;
          if(self.currentFile) {
            if(save) self.currentFile.save();
            self.currentFile = null;
          }
        };

        /**
         * SeashellProject.deleteFile(file)
         *
         * Deletes the given SeashellFile and removes it from the project's
         *  files list.
         */
        SeashellProject.prototype.deleteFile = function(file) {
          var self = this;
          if(file === self.currentFile) self.closeFile(false);

          function rmv(aof) {
            for(var f=0; f<aof.length; f++) {
              if(aof[f] == file) {
                aof.splice(f,1);
                return aof;
              }
              else if(aof[f].children) {
                aof[f].children = rmv(aof[f].children);
              }
            }
            return aof;
          }

          if(file.is_dir) {
            return $q.when(ws.socket.deleteDirectory(self.name, file.fullname()))
              .then(function() { self.files = rmv(self.files); });
          }
          return $q.when(ws.socket.deleteFile(self.name, file.fullname()))
            .then(function() { self.files = rmv(self.files); });
        };

        /**
         * SeashellProject.remove()
         *
         * Deletes the SeashellProject.
         */
        SeashellProject.prototype.remove = function() {
          var self = this;
          var def = $q.defer();
          self.close().then(function() {
            $q.when(ws.socket.deleteProject(self.name))
              .then(function() {
                def.resolve();
              });
            });
          return def;
        };

        /**
         * SeashellProject.compile()
         *
         * Compiles the project.
         */
        SeashellProject.prototype.compile = function() {
          var self = this;
          var def = $q.defer();
          self.save().then(function() {
            $q.when(ws.socket.compileProject(self.name, self.currentFile.fullname(), def));
          });
          //TODO: process results of compilation elsewhere
          return def;
        };

        /**
         * SeashellProject.run(test)
         * 
         * Runs the project. If the test param is set, runs with that test.
         */
        SeashellProject.prototype.run = function(test) {
          var self = this;
          var ext = self.currentFile.ext();
          var comp_prom = ext == "rkt" ? self.save() : self.compile();
          var def = $q.defer();

          $q.when(comp_prom).then(function(msgs) {
            $q.when(ws.socket.runProject(self.name, self.currentFile.fullname(), test ? test : false))
              .then(function(pid) {
                def.resolve(msgs, pid);
              }, function() {
                def.reject(msgs);
              });
          }, function(errs) {
            def.reject(errs);
          });
          return def;
        };

        /**
         * SeashellProject.kill()
         *
         * Kills the current running program.
         */
        SeashellProject.prototype.kill = function() {
          var self = this;
          return $q.when(ws.socket.programKill(self.currentPID)
            .then(function() {
              self.currentPID = null;
            });
        };

        /**
         * SeashellProject.getUploadToken(filename)
         *
         * Gets a file upload token for the given filename.
         */
        SeashellProject.prototype.getUploadToken = function(filename) {
          var self = this;
          return $q.when(ws.socket.getUploadFileToken(self.name, filename));
        };

        /**
         * SeashellProject.onUploadSuccess(filename)
         *
         * Function meant to be used as a callback when file upload is
         *  complete.
         */
        SeashellProject.prototype.onUploadSuccess = function(filename) {
          var self = this;
          self.placeFile(new SeashellFile(self, filename));
        };

        /**
         * SeashellProject.input(input)
         *
         * Sends user input to the current running program's stdin
         */
        SeashellProject.prototype.input = function(input) {
          var self = this;
          return $q.when(ws.socket.programInput(self.currentPID, input));
        };

        /**
         * SeashellProject.getDownloadToken()
         *
         * Gets a project download token.
         */
        SeashellProject.prototype.getDownloadToken = function() {
          var self = this;
          return $q.when(ws.socket.getExportToken(self.name));
        };

        /**
         * SeashellProject.renameFile(file, name)
         *
         * Renames the given file.
         *  - file: the SeashellFile to rename
         *  - name: a path from project root representing the location to rename to
         */
        SeashellProject.prototype.renameFile = function(file, name) {
          var self = this;
          return $q.when(ws.socket.renameFile(self.name, file.fullname(), name))
            .then(function() {
              file.name = name.split("/");
              self.placeFile(file, true);
            });
        };

        /**
         * SeashellProject.getFileFromPath(path)
         *
         * Given a path from project root, returns the corresponding
         *  SeashellFile, or false if it does not exist.
         */
        SeashellProject.prototype.getFileFromPath = function(path) {
          function find(array, p) {
            for(var i=0; i<array.length; i++) {
              if(array[i].name[array[i].name.length-1] == p[0]) {
                if(p.length == 1) return array[i];
                else return find(array[i].children, p.slice(1));
              }
            }
            return false;
          }
          return find(self.files, path.split("/"));
        };

        /**
         * SeashellProject.getTestsForFile(file)
         *
         * Returns a list of tests for the given SeashellFile.
         */
        SeashellProject.prototype.getTestsForFile = function(file) {
          var self = this;
          var testDir = self.getFileFromPath(file.name[0]+"/tests");
          var arr = [];
          if(testDir && testDir.is_dir) {
            for(var i=0; i < testDir.children.length; i++) {
              if(testDir.children[i].ext() == "in") {
                var name = testDir.children[i].name[testDir.children[i].name.length-1];
                name = name.split(".");
                name.pop();
                arr.push(name.join("."));
              }
            }
          }
          return arr;
        };

        /**
         * SeashellProject.exists(fname)
         *
         * Predicate, returns true if the given path exists in the project.
         */
        SeashellProject.prototype.exists = function(fname) {
          var self = this;
          function check(aof) {
            for(var f=0; f<aof.length; f++) {
              if(aof[f].fullname() == fname) return true;
              if(aof[f].is_dir) {
                if(check(aof[f].children)) return true;
              }
            }
            return false;
          }
          return check(self.files);
        }

        /**
         * SeashellProject.currentMarmosetProject()
         *
         * Guesses the appropriate Marmoset project to submit to.
         *  If guessing fails, returns false.
         */
        SeashellProject.prototype.currentMarmosetProject = function() {
          var self = this;
          if(/^a[0-9]+$/i.test(self.name) && /^q[0-9]+[a-z]?$/i.test(self.currentQuestion) {
            var guess = self.name.replace(/^a/i, "A") + self.currentQuestion.replace(/^q/i, "Q");
            var extended = guess+"Extended";
            if(marmoset.projects().indexOf(extended) >= 0)
              return extended;
            if(marmoset.projects().indexOf(guess) >= 0)
              return guess;
          }
          return false;
        };

        /**
         * SeashellProject.submit(marm_project)
         *
         * Submits the current question to the given Marmoset project.
         */
        SeashellProject.prototype.submit = function(marm_project) {
          var self = this;
          var def = $q.defer();
          self.save().then(function() {
            $q.when(ws.socket.marmosetSubmit(self.name, marm_project ,self.currentQuestion))
              .then(function() { def.resolve(); },
                    function() { def.reject(); });
          });
          return def;
        };

      return SeashellProject;})();




      /**
       * Lists projects available.
       *
       * @returns {Angular.$q -> [String]/?} Deferred object that will resolve
       *  to the list of projects available to be opened.
       */
      self.list = function() {
        return $q.when(ws.socket.getProjects());
      };

      /**
       * Fetches new assignments.
       *
       * @returns {Angular.$q -> [projects]/[failed projects]/String} Deferred object
       *  that will resolve
       *  to the list of new assignments cloned, or a list of assignments that failed
       *  to clone, or a error message.
      */
      self.fetch = function() {
        return self.list()
            .then(function (projects) {
              return $q.when($.ajax({url: list_url, dataType: "json"}))
                .catch(function () {
                  return $q.reject("Could not fetch list of skeletons!");
                })
                .then(function (skels) {
                  var new_projects = _.filter(skels,
                      function (skel) {
                        return projects.indexOf(skel) == -1;
                      });
                  var failed_projects = [];
                  var start = $q.defer();
                  start.resolve();
                  return _.foldl(new_projects,
                      function(in_continuation, template) {
                        function clone(failed) {
                          return $q.when(ws.socket.newProjectFrom(template,
                             sprintf(skel_template,
                              template)))
                           .then(function () {
                             if (failed) {
                               return $q.reject("Propagating failure...");
                             }
                           })
                           .catch(function (info) {
                             console.log(sprintf("Could not clone %s! (%s)", template, failed));
                             failed_projects.push(template);
                             return $q.reject("Propagating failure...");
                           });
                        }
                        return in_continuation.then(
                           function () {return clone(false);},
                           function () {return clone(true);}); 
                      },
                      start.promise)
                    .then(function() {return (new_projects);})
                    .catch(function() {return $q.reject(failed_projects);});
                });
            });
      };

      /**
       * Deletes a project.
       * @param {String} name - Name of project.
       * @returns {Angular.$q -> ?} Angular deferred that resolves when
       * the project is deleted.
       */
      self.delete = function (name) {
        return $q.when(ws.socket.deleteProject(name));
      };

      /**
       * Creates a new project.
       *
       * @param {String} name - Name of project.
       * @returns {Angular.$q -> SeashellProject/String}
       *  Angular deferred that resolves to the new, _ready_ SeashellProject instance.
       *  (or a error message on error)
       */
      self.create = function (name) {
        return $q.when(ws.socket.newProject(name)).
          then(function () {
            return (new SeashellProject(name)).init();
          });
      };

      /** 
       * Opens an existing project.
       * @param {String} name
       * @param {bool} read_only - Open read-only?  If set true, 
       *  does not allow write operations on this project, but
       *  also does not lock the project.
       * @param {boolean/optional} force-lock? - Forcibly lock project.
       * @returns {Angular.$q -> SeashellProject/String}
       *  Angular deferred that resolves to the new, _ready_ SeashellProject instance.
       *  (or a error message on error)
       */
      self.open = function(name, read_only, force_lock) {
        if (read_only) {
          return new SeashellProject(name, read_only);
        } else {
          return (new SeashellProject(name)).init(force_lock);
        }
      };
    }]);  
