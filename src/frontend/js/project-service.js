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

        /**
         * SeashellProject.close()
         *
         * Closes a project.
         */
        SeashellProject.prototype.close = function() {
          var self = this;
          return $q.when(ws.socket.unlockProject(self.name)); 
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
            })
            .catch(function() {
              $q.reject("Could not create new directory.");
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
            })
            .catch(function() {
              return $q.reject("Could not open file.");
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
          return $q.when($.when.apply(null, proms))
            .catch(function() {
              return $q.reject("Failed to save project.");
            });
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
