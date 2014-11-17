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

        /**
         * SeashellFile.
         *
         * @constructor Constructor for a new SeashellFile
         * @param {SeashellProject} project - Project for which this file belongs in.
         * @param {String} name - Full name of file.
         * @param {bool} is_dir - Is directory?
         * @param {Number} last_saved - Last saved time.
         */
        function SeashellFile(project, name, is_dir, last_saved) {
          var self = this;
          self.name = name.split("/");
          self.project = project;
          self.children = is_dir ? null : [];
          self.is_dir = is_dir ? true : false;
          self.last_saved = last_saved ? new Date(last_saved) : Date.now();
        }

        /**
         * fullname()
         * Returns the full name of this file.
         */
        SeashellFile.prototype.fullname = function() {
          var self = this;
          return self.name.join("/");
        };

        /**
         * rename()
         * Renames the file, and replaces it in the local directory structure
         * as necessary.
         *
         * @param {String} name - Full path to target.
         */
        SeashellFile.prototype.rename = function(name) {
          var self = this;
          return $q.when(ws.socket.renameFile(self.project.name, self.fullname(), name))
            .then(function() {
              var oldname = self.name.join("/");
              self.name = name.split("/");
              self.project.root._removeFromTree(oldname);
              self.project.root._placeInTree(self);
            });
        };

        /**
         * write(data)
         * Writes data to the file.
         *
         * @param {String} data - Data to write.
         */
        SeashellFile.prototype.write = function(data) {
          var self = this;
          return $q.when(ws.socket.writeFile(self.project.name, self.fullname(), data))
            .then(function() {
              self.last_saved = Date.now();
            });
        };

        /**
         * lastSavedString()
         *
         * Returns a string representing the time this file was last written.
         */
        SeashellFile.prototype.lastSavedString = function() {
          var self = this;
          return self.last_saved.getFullYear() + "-" + (d.getMonth()+1) + "-" + d.getDate() + " " + d.getHours() 
           + ":" + self.last_saved.getMinutes() + ":" + d.getSeconds();
        };

        /**
         * ext()
         *
         * Returns the extension of this file.
         */
        SeashellFile.prototype.ext = function() {
          var self = this;
          return self.name[self.name.length-1].split(".").pop();
        };


        /**
         * _removeFromTree
         *
         * Deletes the local information for the file located at path.
         */
        SeashellFile.prototype._removeFromTree = function(path) {
          var self = this;
          if(path.length == 1) {
            self.children = _.filter(self.children, function(c) {
              return path[0] != c.name[c.name.length-1];
            });
          }
          else {
            _.filter(self.children, function(c) {
              return path[0] == c.name[c.name.length-1];
            })[0]._removeFromTree(path.slice(1));
          }
        };

        /**
         * _placeInTree
         *
         * Places the file locally at path.
         */
        SeashellFile.prototype._placeInTree = function(file, path) {
          var self = this;
          path = path ? path : file.name;
          if(path.length == 1)
            self.children.push(file);
          else {
            _.filter(self.children, function(c) {
              return path[0] == c.name[c.name.length-1];
            })[0]._placeInTree(file, path.slice(1));
          }
        };

        /**
         * find(path)
         * 
         * Finds the file located at path.
         */
        SeashellFile.prototype.find = function(path) {
          var self = this;
          if(path.length == 1) {
            if(self.name[self.name.length-1] == path[0])
              return self;
            return false;
          }
          var res = _.filter(self.children, function(c) {
            return c.is_dir && c.name[c.name.length-1] == path[0];
          });
          return res ? res[0].find(path.slice(1)) : false;
        };

        SeashellProject.prototype._getPath = function(question, folder, fname) {
          if(folder == "common") return "common/"+fname;
          if(folder == "tests") return question+"/tests/"+fname;
          return question+"/"+fname;
        };

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
          var proms = [];
          proms.push(ws.socket.listProject(self.name)
            .then(function(files) {
              self.root = new SeashellFile(self, "", true);
              _.map(files, function(f) {
                root._placeInTree(new SeashellFile(self, f[0], f[1], f[2]));
              });
            }));
          if (!force_lock) {
            proms.push(ws.socket.lockProject(self.name));
          } else {
            proms.push(ws.socket.forceLockProject(self.name));
          }
          return $q.when($.when.apply(proms));
        };

        // Read only functions follow: These do not require the project
        // init function to be called, and therefore only use the raw socket API.
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

        // These functions require .init to be called.
        /**
         * SeashellProject.createFile(fname)
         * 
         * Creates a new file in the project with the given name.
         * Requires .init to be called already.
         */
        SeashellProject.prototype.createFile = function(folder, question, fname, contents) {
          var self = this;
          var path = self.getPath(folder, question, fname);
          var dirp;
          if(folder=="tests") {
            dirp = $q.when(ws.socket.createDirectory(self.project.name, self.currentQuestion+"/tests"));
          }
          else if(folder=="common") {
            dirp = $q.when(ws.socket.createDirectory(self.project.name, "common"));
          }
          else {
            dirp = $q.defer().resolve();
          }
          if(self.root.find(path)) {
            return $q.reject("A file with that name already exists.");
          }
          return dirp.then(function() {
            return $q.when(ws.socket.newFile(self.name, path))
              .then(function() {
                var file = new SeashellFile(self, path);
                self.root.place(file);
                return file.write(contents);
              });
          });
        };

        /**
         * SeashellProject.createDirectory(dname)
         *
         * Creates a new directory in the project at the given path.
         */
        SeashellProject.prototype.createQuestion = function(name) {
          var self = this;
          if(self.root.find(name))
            return $q.reject("Question already exists.");
          return $q.when(ws.socket.newDirectory(self.name, name))
            .then(function() {
              self.root.place(new SeashellFile(self, name, true));
            });
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
        };

        SeashellProject.prototype.openFile = function(path) {
          var self = this;
          if(file.is_dir)
            return $q.reject("Cannot open a directory in editor.");
          self.currentFile = self.root.find(path.split("/"));
          return $q.when(ws.socket.readFile(self.name, path));
        };

        /**
         * SeashellProject.save()
         *
         * Saves all unsaved files in the project.
         */
        SeashellProject.prototype.save = function() {
          return $q.when(self.root.save());
        };

        /**
         * SeashellProject.isUnsaved()
         *
         * Predicate, returns true if there exists an unsaved file
         *  in the project.
         */
        SeashellProject.prototype.isUnsaved = function() {
          var self = this;
          return self.root.isUnsaved();
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
          SeashellProject.currentProject = null;
          return $q.when($.when.apply(proms));
        };

        /**
         * SeashellProject.deleteFile(file)
         *
         * Deletes the given SeashellFile and removes it from the project's
         *  files list.
         */
        SeashellProject.prototype.deleteFile = function(folder, fname) {
          var self = this;
          var path = getPath(self, folder, fname);
          var file = self.root.find(path.split("/"));
          if(!file) {
            return $q.reject("That file does not exist.");
          }
          return $q.when(ws.socket.deleteFile(self.name, path))
            .then(function() {
              self.root.remove(file);
            });
        };

        /**
         * SeashellProject.remove()
         *
         * Deletes the SeashellProject.
         */
        SeashellProject.prototype.remove = function() {
          var self = this;
          return self.close().then(function() {
            return $q.when(ws.socket.deleteProject(self.name));
          });
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
          self.root.place(new SeashellFile(self, filename));
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
          return file.rename(name);
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
