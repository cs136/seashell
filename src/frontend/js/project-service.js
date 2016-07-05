/*
/* Angular bindings for Seashell projects.
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
angular.module('seashell-projects', ['seashell-websocket', 'marmoset-bindings', 'seashell-local-files'])
  /**
   * Project [factory] service.
   * Provides functions to list/load/open/create new SeashellProject
   *  instances.
   */
  .service('projects', ['$rootScope', '$q', 'socket', 'marmoset',
      '$http', 'settings-service', '$cookies',
    function($scope, $q, ws, marmoset, $http, settings, $cookies) {
      "use strict";
      var self = this;
      var CS136_URL = "https://www.student.cs.uwaterloo.ca/~cs136/";
      var CGI_URL = CS136_URL + "cgi-bin/";
      var PROJ_SKEL_URL = CGI_URL + "skeleton_list.cgi";
      var SKEL_ROOT_URL = CS136_URL + "assignment_skeletons/";
      // TODO: update with real template path.
      var PROJ_ZIP_URL_TEMPLATE = SKEL_ROOT_URL + "%s-seashell.zip";
      var PROJ_FILE_LIST_URL_TEMPLATE = CGI_URL + "skeleton_file_list.rkt?template=%s";
      var PROJ_WHITE_LIST_URL = CGI_URL + "project_whitelist.cgi";
      var USER_WHITE_LIST_URL = CGI_URL + "user_whitelist.cgi";
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
          var par = self.name.slice(0,self.name.length-1).join('/');
          if(par.length>0) {
            self.parent = project.root.find(par);
          }
          self.project = project;
          self.children = is_dir ? [] : null;
          self.is_dir = is_dir ? true : false;
          self.last_saved = last_saved ? new Date(last_saved) : Date.now();
        }

        SeashellFile.prototype.toWorker = function() {
          var self = this;
          return self.read().then(function(conts) {
            var obj = { name: self.name[self.name.length-1],
                        contents: conts };
            return obj;
          });
        };

        // for now, bundle everything together from question and common
        SeashellFile.prototype.getDependencies = function() {
          var self = this;
          var deps = [];
          var question = self.project.root.find(self.name.slice(0,1));
          var common = self.project.root.find("common");
          deps = deps.concat(_.filter(question.children, function(f) { return !f.is_dir; }));
          if(common)
            deps = deps.concat(common.children);
          return $q.all(_.map(deps, function(d) { return d.read(); }))
            .then(function() { return deps; });
        };

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
          return $q.when(ws.renameFile(self.project.name, self.fullname(), name))
            .then(function() {
              var oldname = self.name.join("/");
              self.project.root._removeFromTree(oldname.split("/"), true);
              self.name = name.split("/");
              self.project.root._placeInTree(self, self.name, true);
            });
        };

        /**
         * read()
         * Reads the file and returns a deferred that will resolve to its contents.
         * If offline, it will attempt to read the offline store.
         * This function does not attempt to do any synchronization (see sync instead).
         */
        SeashellFile.prototype.read = function() {
          var self = this;

          return $q.when(ws.readFile(self.project.name, self.fullname()))
            .then(function(conts) {
              if(conts === null) {
                  return $q.reject(self.fullname() + ": Could not read file from server and no local copy exists.");
                }
              return {data: conts.data, history: conts.history};
            });
        };

        /**
         * write(data, history)
         * Writes data to the file.
         *
         * @param {String} data - Data to write.
				 * @param {String} history - the file's undoHistory 
         *   (to be placed in a corresponding hidden <FILENAME>.history file)
         */
        SeashellFile.prototype.write = function(data, history) {
          var self = this;
          return $q.when(ws.writeFile(self.project.name, self.fullname(), data, history));
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
          if (path.length == 1) {
            var split = _.groupBy(self.children, function(c) {
              return path[0] != c.name[c.name.length-1] ? 1 : 0;
            });
            if (!split[0]) {
              return $q.reject("File does not exist.");
            }
            self.children = split[1] || [];
            if (!soft_delete) {
              return $q.when(ws.deleteFile(self.project.name, split[0][0].fullname()));
            }
          }
          else {
            return _.filter(self.children, function(c) {
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
        SeashellFile.prototype._placeInTree = function(file, path, soft_place,
          contents, encoding, normalize) {
          var self = this;
          path = path ? path : file.name;
          if (path.length == 1) {
            if (!soft_place) {
              if(file.is_dir) {
                return $q.when(ws.newDirectory(file.project.name,
                  file.fullname())).then(function () {
                  self.children.push(file);
                });
              } else {
                return $q.when(ws.newFile(file.project.name,
                  file.fullname(), contents, encoding, normalize ? true : false))
                    .then(function () {
                      self.children.push(file);
                    });
              }
            } else {
              self.children.push(file);
            }
          } else {
            var match = _.filter(self.children, function(c) {
              return path[0] == c.name[c.name.length-1];
            });
            if (match.length>0) {
              return match[0]._placeInTree(file, path.slice(1), soft_place,
                contents, encoding, normalize);
            } else {
              var dir = new SeashellFile(file.project, file.name.slice(0,file.name.length-path.length+1).join('/'), true);
              return (dir.fullname === "" ? $q.when() :
                  $q.when(ws.newDirectory(dir.project.name, dir.fullname())))
                .then(function() {
                  self.children.push(dir);
                  return self._placeInTree(file, path, soft_place, contents,
                    encoding, normalize);
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

        SeashellProject.prototype._buildTree = function() {
          var self = this;
          return $q.when(ws.listProject(self.name)).then(function(files) {
            self.root = new SeashellFile(self, "", true);
            var chain = function(i) {
              if(i>= files.length) return $q.when();
              var f = files[i];
              return $q.when(self.root._placeInTree(new SeashellFile(self, f[0], f[1], f[2], f[3]), null, true))
                .then(function() {
                  return chain(i+1);
                });
            };
            return chain(0);
          });
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
            result = $q.when(ws.lockProject(self.name));
          } else if (lock === "force-lock") {
            result = $q.when(ws.forceLockProject(self.name));
          } else {
            result = $q.when();
          }

          return result.then(function() { return self._buildTree(); }).then(function () {
               /* If the project is listed in the project skeleton on the server,
                  set self.projectZipURL to the project directory url.
                  set self.skel to the file skeleton url.
                  eg. self.projectZipURL = "https://.....~cs136/assignments_skeleton/A0/";
                 */
               self.inSkeleton().then(function(bool) {
                  if (! bool) {return;}
                  self.projectZipURL = sprintf(PROJ_ZIP_URL_TEMPLATE, self.name);
                  self.skelURL = sprintf(PROJ_FILE_LIST_URL_TEMPLATE, self.name);
                  self.pullMissingSkelFiles();
               });
               return self;
            });
          self.cb_key = ws.register_callback('connected', self._buildTree);
        };

        /* List all file in this project.

          Return type: [String] - paths
          eg. ["q1a/file.c", "q1a/tests/a.in","common/text.txt"]
        */

        SeashellProject.prototype.list = function() {
          var self = this;
          var files = [];
          var restDirs = [self.root];
          while (restDirs.length > 0) {
            var dir = restDirs.pop();
            dir.children.forEach(helper);
          }
          function helper(c) {
            if (c.children) {
              restDirs.push(c);
            } else {
              files.push(c.name.join('/'));
            }
          }
          return files;
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
        SeashellProject.prototype.createFile = function(folder, question, fname, contents, encoding, normalize) {
          var self = this;
          var path = self._getPath(question, folder, fname);
          contents = contents || "";

          if(self.root.find(path)) {
            return $q.reject("A file with that name already exists.");
          }
          var file = new SeashellFile(self, path, contents);
          return self.root._placeInTree(file, false, false, contents, encoding, normalize);
        };


        SeashellProject.prototype.createQuestion = function(question) {
          var self = this;
          if(self.root.find(question)) {
            return $q.reject("A question with that name already exists.");
          }
          var dir = new SeashellFile(self, question, null, true);
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
            return $q.reject("Cannot open file! " + filename);
          if(file.is_dir)
            return $q.reject("Cannot open a directory in editor.");
          return file.read();
        };

        /**
         * SeashellProject.hasFile(question, folder, filename)
         *
         * Returns if the file exists.
         */
        SeashellProject.prototype.hasFile = function(question, folder, filename) {
          var self = this;
          var file = self.root.find(self._getPath(question, folder, filename));
          return file && !file.is_dir;
        };

        /**
         * Seashellproject.saveFile(question, folder, filename)
         *
         * Saves the file at the given location.
         */
        SeashellProject.prototype.saveFile = function(question, folder, filename, data, history) {
          var self = this;
          var file = self.root.find(self._getPath(question, folder, filename));
          if(!file)
            return $q.reject("Cannot save nonexistant file");
          if(file.is_dir)
            return $q.reject("Cannot save a directory.");
          return file.write(data, history);
        };

        /**
         * SeashellProject.close(save)
         *
         * Closes the project.
         */
        SeashellProject.prototype.close = function(save) {
          var self = this;
          ws.unregister_callback(self.cb_key);
          if (self.lock === "none") {
            return $q.when();
          }
          return $q.when(ws.unlockProject(self.name));
        };

        /**
         * SeashellProject.hasQuestion(question)
         *
         * Returns if a question exists.
         */
        SeashellProject.prototype.hasQuestion = function (question) {
          var self = this;
          var res = self.root.find(self._getPath(question));
          return res && res.is_dir;
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
          return self.root._removeFromTree(path.split("/")).then(function() {
            return self.inSkeleton().then(function(bool) {
               return bool && self.pullMissingSkelFiles();
            });
          });
        };

        /**
         * SeashellProject.mostRecentlyUsed(question)
         *
         * @param {String} question - question or null for the project.
         *
         * Returns the most recently used question/file given
         * a question/project.
         */
        SeashellProject.prototype.mostRecentlyUsed = function (question) {
          var self = this;
          return $q.when(ws.getMostRecentlyUsed(self.name, (question && self._getPath(question)) || false))
            .then(function (recent) {
              if (recent) {
                if (question && self.hasFile(question, recent.part, recent.file)) {
                  return recent;
                } else if (!question && self.hasQuestion(recent)) {
                  return recent;
                } else {
                  return null;
                }
              }
              return null;
            });
        };

        /**
         * SeashellProject.updateMostRecentlyUsed(question, part, file)
         *
         * @param {String} question/part/file - Most recently used file.
         *
         * Updates the most recently used file.
         */
        SeashellProject.prototype.updateMostRecentlyUsed = function (question, part, file) {
          var self = this;
          var qpath = "";
          if (question && part && file) {
            var path = self._getPath(question, part, file);
            qpath = self._getPath(question);

            return $q.all([ws.updateMostRecentlyUsed(self.name, false, ["dexists", qpath], question),
                           ws.updateMostRecentlyUsed(self.name, qpath, ["fexists", path], {part: part, file: file})]);
          } else if (question) {
            qpath = self._getPath(question);
            return ws.updateMostRecentlyUsed(self.name, false, ["dexists", qpath], question);
          }
        };

        /**
         * SeashellProject.getFileToRun(question)
         *
         * Returns the basename of the file to run when hitting run, from the
         * question settings file.
         */
        SeashellProject.prototype.getFileToRun = function (question) {
          var self = this;
          return $q.when(ws.getFileToRun(self.name, question))
            .then(function (result) {
                self.fileToRun = result;
                return result;
            });
        };


        /**
         * SeashellProject.setFileToRun(question, folder, file)
         *
         * Modify the settings file to set which file to run.
         */
        SeashellProject.prototype.setFileToRun = function (question, folder, file) {
          var self = this;
          return $q.when([ws.setFileToRun(self.name, question, folder, file)])
            .then(function() { self.fileToRun = file; });
        };

        /**
         * SeashellProject.remove()
         *
         * Deletes the SeashellProject.
         */
        SeashellProject.prototype.remove = function() {
          var self = this;
          return self.close().then(function() {
            return $q.when(ws.deleteProject(self.name));
          });
        };

        /**
         * SeashellProject.run(...)
         * Compiles [if necessary] and runs the project.
         *
         * test - boolean parameter, run with tests if true.
         */
        SeashellProject.prototype.run = function(question, test) {
          var self = this;
          // TODO: handle racket files.
          var tests = test ? self.getTestsForQuestion(question) : [];
          var path = self._getPath(question, "question", self.fileToRun);
          var file = self.root.find(path);

          if(!file) {
            return $q.reject("Attempting to run file that does not exist.");
          }
          if (test && tests.length === 0)
            return $q.reject("No tests for question!");

          return $q.when(ws.compileAndRunProject(self.name, question, file, tests));
        };

        /**
         * save(message | optional)
         * Saves (commits) the project to Git.
         */
        SeashellProject.prototype.save = function(message) {
          var self = this;
          return $q.when(ws.saveProject(self.name, message));
        };

        /**
         * SeashellProject.kill()
         *
         * Kills program with specified PID.
         */
        SeashellProject.prototype.kill = function(pid) {
          return $q.when(ws.programKill(pid));
        };

        /**
         * SeashellProject.getUploadToken(filename)
         *
         * Gets a file upload token for the given filename.
         */
        SeashellProject.prototype.getUploadToken = function(question, folder, filename) {
          var self = this;
          return $q.when(ws.getUploadFileToken(self.name, self._getPath(question, folder, filename)));
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
          return $q.when(ws.getExportToken(self.name));
        };

        /**
         * SeashellProject.renameFile(file, name)
         *
         * Renames the given file.
         */
        SeashellProject.prototype.renameFile = function(question, folder, file, newFolder, newName) {
          var self = this;
          var path = self._getPath(question, folder, file);
          var target = newName;
          if (self.root.find(path)) {
            return self.root.find(path).rename(target).then(function() {
               return self.inSkeleton().then(function(bool) {
                  return bool && self.pullMissingSkelFiles();
               });
            });
          } else {
            return $q.reject("No file found!");
          }
        };


        /**
         * SeashellProject.getTestsForQuestion(question)
         *
         * Returns a list of tests for the given question.
         */
        SeashellProject.prototype.getTestsForQuestion = function(question) {
          var self = this;
          var testDir = self.root.find(question+"/tests");
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
          // first test if the project/question is an assignment
          if(/^a[0-9]+$/i.test(self.name) && /^(a\d)?(q|p)[0-9]+[a-z]*$/i.test(question)) {
            var withoutA = self.name.substr(1);    // eg. "A5" -> "5"
            var withoutQ = question.substr(1);     // eg. "q3b" -> "3b"

            // construct two case-insensitive regexes
            // match several possibilities:
            // - A2p5b
            // - A5q5b
            var regexFormat = sprintf("^a%s(q|p)%s", withoutA, withoutQ);
            var guess = new RegExp(regexFormat, "i");
            var extended = new RegExp(regexFormat + "extended", "i");
            return marmoset.projects().then(function(projects) {
              return $q.when(
                // search for extended first
                _.find(projects, function(p) {
                  return extended.test(p);
                }) ||
                // then search for non-extension
                _.find(projects, function(p) {
                  return guess.test(p);
                })
              );
            });
          }
          return $q.when(false);
        };

        /**
         * SeashellProject.submit(question, project)
         *
         * Submits the current question to the given Marmoset project.
         */
        SeashellProject.prototype.submit = function(question, project) {
          var self = this;
          return $q.when(ws.marmosetSubmit(self.name, project, question));
        };

        SeashellProject.prototype.sendInput = function(pid, message) {
          var self = this;
          // handle offline mode:
          if(ws.isOffline()) {
            self.runner.postMessage(message);
            var def = $q.defer();
            def.resolve();
            return def.promise;
          }
          return $q.when(ws.programInput(pid, message));
        };

        SeashellProject.prototype.sendEOF = function(pid) {
          var self = this;
          return $q.when(ws.sendEOF(pid));
        };

        /* Returns a list of file paths in the current project skeleton.
           This method only send requests to the server once when it's initially called.
           The server response is remembered for future calls.

           Deferred return type: [String] -- list of file paths, relative to the project directory.
           eg. ["q1a/file.c", "q1a/tests/a.in","common/text.txt"]
        */
        SeashellProject.prototype.listSkelFiles = function() {
          var self = this;
          if (! self._listSkelFiles) {
            self._listSkelFiles = $http({url: self.skelURL}).then(function(result) {
              return result.data.result.map(function(path) {
                // remove the project name from the start
                return path.replace(new RegExp("^"+self.name+"/"), "");
              }).filter(function(path) {
                // remove paths with trailing slash (only want files, no directory)
                return path.length > 0 && path[path.length-1] !== '/';
              }).sort();
            });
          }
          return self._listSkelFiles;
        };

        /**
         * Checks if the project is listed in the skeleton on the server.
         * This function is memoized.
        */
        SeashellProject.prototype.inSkeleton = function() {
          var self = this;
          if (! self._inSkeleton) {
            self._inSkeleton = listSkelProjects().then(function(names) {
              return false || _.find(names, function(a){return a === self.name;});
            });
          }
          return self._inSkeleton;
        };

        /* Returns a list of files missing in the local project,
           by comparing with the server project skeleton.

           This function uses SeashellProject.prototype.listSkelFiles
           to get a list of files on the server.

           Deferred return type: [String] -- list of missing local files
        */
        SeashellProject.prototype.missingSkelFiles = function() {
          var self = this;
          return $q.all([self.list(), self.listSkelFiles()]).then(function(results) {
            var localFileList = results[0];
            var serverFileList = results[1];
            return _.filter(serverFileList, function(serverFile) {
              return ! _.find(localFileList, function(localFile) {return localFile === serverFile;});
            });
          });
        };

        /* Calls SeashellProject.prototype.missingSkelFiles to get a list of
           missing files, then requests the server to create them, then reads
           the files from the server.
        */
        SeashellProject.prototype.pullMissingSkelFiles = function() {
          var self = this;
          if(ws.isOffline()) {
            return $q.when(true);
          }
          return self.missingSkelFiles().then(function(missingFiles) {
              if (missingFiles.length) {
                return $q.all(missingFiles.map(function(fpath) {
                    return $q.when(ws.restoreFileFrom(self.name, fpath, self.projectZipURL)).then(function() {
                        // now soft create these files in the front end and read.
                        var file = new SeashellFile(self, fpath, false);
                        return file.read().then(function(contents) {
                            return self.root._placeInTree(file, false, true, contents);
                        });
                    });
                }));
              }
          });
        };
      return SeashellProject;})();


      /**
       * Lists projects available.
       * @returns {Angular.$q -> [String]/?} Deferred object that will resolve
       *  to the list of projects available to be opened.
       */
      self.list = function() {
        return $q.when(ws.getProjects());
      };

      /**
       * Returns a memoized list of project template skeletons.
      */
      self.listSkelProjects = listSkelProjects;
      var listSkelProjects = function() {
        if (! self._listSkelProjects) {
           self._listSkelProjects = $http({url: PROJ_SKEL_URL}).catch(function (reason) {
              return $q.reject("Could not fetch list of skeletons: " + reason);
           }).then(function(result) {
              return result.data;
           });
        }
        return self._listSkelProjects;
      };

      /**
       * Returns a memoized list of whitelisted users.
       */
      self.userWhitelist = function() {
         if (! self._userWhitelist) {
            self._userWhitelist = $http.get(USER_WHITE_LIST_URL).then(function(result) {
               return result.data;
            }).catch(function(err) {
               console.warn("Could not access the user whitelist. Assuming it's empty. Received:", err);
               return [];
            });
         }
         return self._userWhitelist;
      };

      /**
       * Returns a memoized lists of whitelisted projects.
       */
      self.projectWhitelist = function() {
         if (! self._projectWhitelist) {
            self._projectWhitelist = $http.get(PROJ_WHITE_LIST_URL).then(function(result) {
               return result.data;
            }).catch(function(err) {
               console.warn("Could not access the project whitelist. Assuming it's empty. Received:", err);
               return [];
            });
         }
         return self._projectWhitelist;
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
         return self.list().then(function (projects) {
            return $http({url: PROJ_SKEL_URL}).catch(function () {
               return $q.reject("Could not fetch list of skeletons!");
            }).then(function (results) {
               var localProjects = projects.map(function(v, k) {return v[0];}).sort();
               // expects a list of project (assignment) names : (listof String)
               var skels = results.data.sort();
               var user = $cookies.getObject(SEASHELL_CREDS_COOKIE).user;
               return self.userWhitelist().then(function(usernames) {
                  if (_.contains(usernames, user)) {
                     return self.projectWhitelist().then(function(more) {
                        skels = skels.concat(more);
                     });
                  }
               }).finally(function() {
                  var new_projects = _.difference(skels, localProjects);
                  var failed_projects = [];
                  var start = $q.when();
                  return _.foldl(new_projects, function(in_continuation, template) {
                     function clone(failed) {
                        return $q.when(ws.newProjectFrom(template,
                           sprintf(PROJ_ZIP_URL_TEMPLATE, template))).then(function () {
                              if (failed) {
                                 return $q.reject("Propagating failure...");
                              }
                           }).catch(function (info) {
                             failed_projects.push(template);
                             return $q.reject("Propagating failure...");
                           });
                        }
                        return in_continuation.then(function () {return clone(false);},
                                                    function () {return clone(true);});
                  }, start).then(function() {return new_projects;})
                           .catch(function() {return $q.reject(failed_projects);});
               });
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
        return $q.when(ws.deleteProject(name));
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
        return $q.when(ws.newProject(name)).
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
