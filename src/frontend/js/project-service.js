/*u
 * Angular bindings for Seashell projects.
 * Copyright (C) 2013-2015 The Seashell Maintainers.
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
  .service('projects', ['$rootScope', '$q', 'socket', 'marmoset', '$http',
    function($scope, $q, ws, marmoset, $http) {
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
        function SeashellProject (name) {
          var self = this;

          self.name = name;
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
          self.children = is_dir ? [] : null;
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
         * filename()
         * Returns the name of this file.
         */
        SeashellFile.prototype.filename = function () {
          var self = this;
          return self.name[self.name.length - 1];
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
              self.project.root._removeFromTree(oldname.split("/"), true);
              self.name = name.split("/");
              self.project.root._placeInTree(self, self.name, true);
            });
        };

        SeashellFile.prototype.read = function() {
          var self = this;
          return $q.when(ws.socket.readFile(self.project.name, self.fullname()))
            .then(function (conts) {
              return conts;
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
          return $q.when(ws.socket.writeFile(self.project.name, self.fullname(), data));
        };

        /**
         * lastSavedString()
         *
         * Returns a string representing the time this file was last written.
         */
        SeashellFile.prototype.lastSavedString = function() {
          var self = this;
          return self.last_saved.toUTCString();
        };

        /**
         * ext()
         *
         * Returns the extension of this file.
         */
        SeashellFile.prototype.ext = function() {
          var self = this;
          return self.filename().split(".").pop();
        };


        /**
         * _removeFromTree
         *
         * Deletes the file located at path.
         * Default is to delete the file in the backend as well.
         * Set soft_delete to true to prevent backend deletion.
         */
        SeashellFile.prototype._removeFromTree = function(path, soft_delete) {
          var self = this;
          if(path.length == 1) {
            var split = _.groupBy(self.children, function(c) {
              return path[0] != c.name[c.name.length-1] ? 1 : 0;
            });
            if(!split[0])
              return $q.reject("File does not exist.");
            self.children = split[1];
            if(!soft_delete)
              return $q.when(ws.socket.deleteFile(self.project.name, split[0][0].fullname()));
          }
          else {
            _.filter(self.children, function(c) {
              return path[0] == c.name[c.name.length-1];
            })[0]._removeFromTree(path.slice(1), soft_delete);
          }
            return $q.when();
        };

        /**
         * _placeInTree
         *
         * Places the file in the directory structure at path.
         * By default, actually creates a file at that path. If soft_place is true,
         *  does not create the file in the backend.
         */
        SeashellFile.prototype._placeInTree = function(file, path, soft_place, contents, encoding) {
          var self = this;
          path = path ? path : file.name;
          if(path.length == 1) {
            if(!soft_place) {
              if(file.is_dir)
                return $q.when(ws.socket.newDirectory(file.project.name, file.fullname())).then(function () {
                  self.children.push(file);
                });
              else
                return $q.when(ws.socket.newFile(file.project.name, file.fullname(), contents, encoding)).then(function () {
                  self.children.push(file);
                });
            } else {
              self.children.push(file);
            }
          }
          else {
            var match = _.filter(self.children, function(c) {
              return path[0] == c.name[c.name.length-1];
            });
            if(match.length>0)
              return match[0]._placeInTree(file, path.slice(1), soft_place, contents, encoding);
            else {
              var dir = new SeashellFile(file.project, file.name.slice(0,file.name.length-path.length+1).join('/'), true);
              return (dir.fullname == "" ? $q.when() : 
                  $q.when(ws.socket.newDirectory(dir.project.name, dir.fullname())))
                .then(function() {
                  self.children.push(dir);
                  return self._placeInTree(file, path, soft_place, contents, encoding);
                });
            }
          }
          return $q.when();
        };

        /**
         * find(path)
         * 
         * Finds the file located at path.
         */
        SeashellFile.prototype.find = function(path) {
          var self = this;

          // Two cases: path is a string, or an array:
          if (typeof path === "string") {
            return self.find(_.filter(path.split("/"), function (s) {return s.length > 0;}));
          } else {
            if (self.is_dir && path.length > 0) {
              var res = _.filter(self.children, function(c) {
                return c.filename() === path[0];
              });
              return res.length > 0 ? res[0].find(path.slice(1)) : false;
            }
            else if(path.length === 0) {
              return self;
            } else {
              return false;
            }
          }
        };

        /**
         * list()
         *
         * Returns all children.
         */
        SeashellFile.prototype.list = function () {
          var self = this;
          return self.children;
        };

        SeashellProject.prototype._getPath = function(question, folder, fname) {
          folder = folder || "";
          fname = fname || "";

          if(folder == "common") return "common/"+fname;
          if(folder == "tests") return question+"/tests/"+fname;
          return question+"/"+fname;
        };

        /**
         * SeashellProject.init()
         *
         * Initializes a project.
         * @param {String} lock - one of "lock", "force-lock", "none"
         * @returns {Angular.$q} Deferred that resolves when this is done,
         *  or an error:
         *    "locked" - Project is locked.
         *    other    - Error message.
         */
        SeashellProject.prototype.init = function(lock) {
          var self = this;
          lock = lock || "lock";
          self.lock = lock;

          var result = null;
          if (lock === "lock") {
            result = $q.when(ws.socket.lockProject(self.name));
          } else if (lock === "force-lock") {
            result = $q.when(ws.socket.forceLockProject(self.name));
          } else {
            result = $q.when();
          }

          result = result.then(function () {
            return $q.when(ws.socket.listProject(self.name))
            .then(function(files) {
              self.root = new SeashellFile(self, "", true);
              _.map(files, function(f) {
                self.root._placeInTree(new SeashellFile(self, f[0], f[1], f[2]), null, true);
              });
            });});
          return result.then(function () {return self;});
        };

        /** SeashellProject.questions()
         *
         * Lists all questions.
         * @returns [String] List of questions.
         */
        SeashellProject.prototype.questions = function() {
          var self = this;
          return _.map(_.filter(self.root.list(),
                                function (f) {return f.is_dir && f.filename() != "common";}),
                       function (f) {return f.filename();});
        };

        /**
         * SeashellProject.filesFor(question)
         *
         * Lists all files present in a question.
         */
        SeashellProject.prototype.filesFor = function(question) {
          var self = this;
          var common = self.root.find(self._getPath(question, "common")) ?
                      _.map(_.filter(self.root.find(self._getPath(question, "common")).list(),
                                      function (f) {return !f.is_dir;}),
                             function (f) {return f.filename();}) : [];
          var quest = _.map(_.filter(self.root.find(self._getPath(question)).list(),
                                      function (f) {return !f.is_dir;}),
                             function (f) {return f.filename();});
          var tests = self.root.find(self._getPath(question, "tests")) ?
                      _.map(_.filter(self.root.find(self._getPath(question, "tests")).list(),
                                      function (f) {return !f.is_dir;}),
                             function (f) {return f.filename();}) : [];
          return {common: common, question: quest, tests: tests};
        };

        /**
         * SeashellProject.createFile(fname)
         * 
         * Creates a new file in the project with the given name.
         * Requires .init to be called already.
         *
         */
        SeashellProject.prototype.createFile = function(folder, question, fname, contents, encoding) {
          var self = this;
          var path = self._getPath(question, folder, fname);
          contents = contents || "";
          
          if(self.root.find(path)) {
            return $q.reject("A file with that name already exists.");
          }
          var file = new SeashellFile(self, path);
          return self.root._placeInTree(file, false, false, contents, encoding);
        };

        SeashellProject.prototype.createQuestion = function(question) {
          var self = this;
          if(self.root.find(question)) {
            return $q.reject("A question with that name already exists.");
          }
          var dir = new SeashellFile(self, question, true);
          return self.root._placeInTree(dir, false, false);
        };

        /**
         * SeashellProject.openFile(question, folder, filename)
         *
         * Reads the file located at the given path, relative to the
         *  project root.
         */
        SeashellProject.prototype.openFile = function(question, folder, filename) {
          var self = this;
          var file = self.root.find(self._getPath(question, folder, filename));
          if(!file)
            return $q.reject("Cannot open file!");
          if(file.is_dir)
            return $q.reject("Cannot open a directory in editor.");
          return file.read();
        };

        /**
         * Seashellproject.saveFile(question, folder, filename)
         *
         * Saves the file at the given location.
         */
        SeashellProject.prototype.saveFile = function(question, folder, filename, data) {
          var self = this;
          var file = self.root.find(self._getPath(question, folder, filename));
          if(!file)
            return $q.reject("Cannot save nonexistant file");
          if(file.is_dir)
            return $q.reject("Cannot save a directory.");
          return file.write(data);
        };

        /**
         * SeashellProject.close(save)
         *
         * Closes the project.
         */
        SeashellProject.prototype.close = function(save) {
          var self = this;
          
          if (self.lock === "none") {
            return $q.when();
          }
          return $q.when(ws.socket.unlockProject(self.name));
        };

        /**
         * SeashellProject.deleteFile(file)
         *
         * Deletes the given SeashellFile and removes it from the project's
         *  files list.
         */
        SeashellProject.prototype.deleteFile = function(question, folder, fname) {
          var self = this;
          var path = self._getPath(question, folder, fname);
          return self.root._removeFromTree(path.split("/"));
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
         * SeashelLProject.run(...)
         * Compiles [if necessary] and runs the project.
         *
         * test - boolean parameter, run with tests if true.
         */
        SeashellProject.prototype.run = function(question, folder, filename, data, test) {
          var self = this;
          // TODO: handle racket files.
          var file = self.root.find(self._getPath(question, folder, filename));
          var tests = test ? self.getTestsForFile(file) : [];
          return $q.when(ws.socket.compileAndRunProject(self.name, file.fullname(), tests));
        };

        /** 
         * save(message | optional)
         * Saves (commits) the project to Git.
         */
        SeashellProject.prototype.save = function(message) {
          var self = this;
          return $q.when(ws.socket.saveProject(self.name, message));
        };

        /**
         * SeashellProject.kill()
         *
         * Kills program with specified PID.
         */
        SeashellProject.prototype.kill = function(pid) {
          var self = this;
          return $q.when(ws.socket.programKill(pid));
        };

        /**
         * SeashellProject.getUploadToken(filename)
         *
         * Gets a file upload token for the given filename.
         */
        SeashellProject.prototype.getUploadToken = function(question, folder, filename) {
          var self = this;
          return $q.when(ws.socket.getUploadFileToken(self.name, self._getPath(question, folder, filename)));
        };

        /**
         * SeashellProject.onUploadSuccess(filename)
         *
         * Function meant to be used as a callback when file upload is
         *  complete.
         */
        SeashellProject.prototype.onUploadSuccess = function(filename) {
          var self = this;
          self.root._placeInTree(new SeashellFile(self, filename), true);
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
        SeashellProject.prototype.renameFile = function(question, folder, file, newFolder, newName) {
          var self = this;
          var path = self._getPath(question, folder, file);
          var target = self._getPath(question, newFolder, newName); 
          if (self.root.find(path))
            return self.root.find(path).rename(target);
          else
            return $q.reject("No file found!");
        };


        /**
         * SeashellProject.getTestsForFile(file)
         *
         * Returns a list of tests for the given SeashellFile.
         */
        SeashellProject.prototype.getTestsForFile = function(file) {
          var self = this;
          var testDir = self.root.find(file.name[0]+"/tests");
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
         * SeashellProject.currentMarmosetProject()
         *
         * Guesses the appropriate Marmoset project to submit to.
         *  If guessing fails, returns false.
         */
        SeashellProject.prototype.currentMarmosetProject = function(question) {
          var self = this;
          if(/^a[0-9]+$/i.test(self.name) && /^q[0-9]+[a-z]?$/i.test(question)) {
            var guess = self.name.replace(/^a/i, "A") + question.replace(/^q/i, "P");
            var extended = guess+"Extended";
            if(marmoset.projects().indexOf(extended) >= 0)
              return extended;
            if(marmoset.projects().indexOf(guess) >= 0)
              return guess;
          }
          return false;
        };

        /**
         * SeashellProject.submit(question, project)
         *
         * Submits the current question to the given Marmoset project.
         */
        SeashellProject.prototype.submit = function(question, project) {
          var self = this;
          return $q.when(ws.socket.marmosetSubmit(self.name, project || self.currentMarmosetProject(question), question));
        };

        SeashellProject.prototype.sendInput = function(pid, message) {
          var self = this;
          return $q.when(ws.socket.programInput(pid, message));
        };

        SeashellProject.prototype.sendEOF = function(pid) {
          var self = this;
          return $q.when(ws.socket.sendEOF(pid));
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
              return $http({url: list_url})
                .catch(function () {
                  return $q.reject("Could not fetch list of skeletons!");
                })
                .then(function (results) {
                  var skels = results.data;
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
       * @param {bool} return_project - Return new project or not?
       * @returns {Angular.$q -> SeashellProject/String}
       *  Angular deferred that resolves to the new, _ready_ SeashellProject instance.
       *  (or a error message on error)
       */
      self.create = function (name, return_project) {
        return $q.when(ws.socket.newProject(name)).
          then(function () {
            if (return_project)
              return (new SeashellProject(name)).init();
          });
      };

      /** 
       * Opens an existing project.
       * @param {String} name
       * @param {string} One of "lock", "none", "force-lock"
       * @returns {Angular.$q -> SeashellProject/String}
       *  Angular deferred that resolves to the new, _ready_ SeashellProject instance.
       *  (or a error message on error)
       */
      self.open = function(name, lock) {
        return (new SeashellProject(name)).init(lock);
      };
    }]);
