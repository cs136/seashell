angular.module('seashell-local-files', [])
  /**
   * Local file storage service, using localforage.js
   * Must call init before using!
   */
  .service('localfiles', ['$q', '$cookies',
    function($q, $cookies) {
      "use strict";
      var self = this;

      self.user = null;   // username
      self.store = null;  // localForage instance
      self.projects = []; // offline storage of all project trees
      self.offlineChangelog = []; // array of OfflineChange objects 
      self.offlineChangelogSet = {}; // properties determine membership in offlineChangelog
      self.offlineDeletedFiles = {}; // set of files that have been deleted


      /* Constructor for an OfflineChange
       * It stores information about a file that has changed offline
       *   but not online, so that it can be updated when the user goes 
       *   back online.
       */
      var OfflineChange = function(project, path, isDeleted) {
        var self = this;
        self._project = project;
        self._path = path;
        self._isDeleted = !!isDeleted;
      };

      // Getter for project name. Returns a string.
      OfflineChange.prototype.getProject = function() {
        var self = this;
        return self._project;
      };

      // Getter for path. Returns a string.
      OfflineChange.prototype.getPath = function() {
        var self = this;
        return self._path;
      };

      // Returns true if the change was a deletion.
      OfflineChange.prototype.isDeleted = function() {
        var self = this;
        return self._isDeleted;
      };

      /*
        Returns a list of changes since last sync to be sent to the backend
        on reconnection. Returns array of entries of the form
          ["deleteFile"/"editFile", {project: ..., path: ..., contents: ...,
            checksum: ...}]
      */
      self.getOfflineChanges = function() {
        var proms = [];
        // edited files
        _.each(self.offlineChangelog, function (oc) {
          proms.push($q.when(self.readFile(oc.getProject(), oc.getPath()))
            .then(function(contents) {
              return ["editFile", {project: oc.getProject(), path:oc.getPath(),
                                   contents: contents.contents, checksum: contents.offline_checksum}];
            }));
        });

        // deleted files
        _.each(self.offlineDeletedFiles, function(oc) {
          proms.push($q.when(["deleteFile", {project: oc.getProject(), path:oc.getPath()}]));
        });
        return $q.all(proms);
      };

      // Add a change to the offline changelog.
      // Does nothing if the change is already logged.
      self._addOfflineChange = function(project, path) {
        var self = this;
        var key = sprintf("%s/%s", project, path);

        var promises = [];

        delete self.offlineDeletedFiles[key];
        if (!(key in self.offlineChangelogSet)) {
          self.offlineChangelogSet[key] = true;
          self.offlineChangelog.push(new OfflineChange(project, path));
          return $q.all([
            $q.when(self.store.setItem("//offlineChangelog", self.offlineChangelog)),
            $q.when(self.store.setItem("//offlineDeletedFiles", self.offlineDeletedFiles))
            ]);
        } else {
          return $q.when();
        }
      };

      // Add a file to the offline delete log.
      // Does nothing if the change has already been logged.
      self._addOfflineDelete = function(project, path) {
        var self = this;
        var key = sprintf("%s/%s", project, path);

        var promises = [];

        // remove from offlineChangelog
        // since we won't be deleting often, this is acceptable
        if (key in self.offlineChangelogSet) {
          console.log("Removing from offlineChangelog because we deleted");
          delete self.offlineChangelogSet[key];
          self.offlineChangelog = _.reject(self.offlineChangelog,
            function(oc) {
              return oc.getProject() === project && oc.getPath() == path;
            });
          promises.push($q.when(self.store.setItem("//offlineChangelog", self.offlineChangelog)));
        }

        if (!(key in self.offlineDeletedFiles)) {
          self.offlineDeletedFiles[key] = new OfflineChange(project, path, true);
          promises.push($q.when(self.store.setItem("//offlineDeletedFiles", self.offlineDeletedFiles)));
          return $q.all(promises);
        } else {
          return $q.when();
        }
      };

      // Must call this before using anything
      // Returns a deferred that resolves to true when initialization is complete.
      self.init = function() {
        var self = this;
        self.user = $cookies.getObject(SEASHELL_CREDS_COOKIE).user;

        // set up localforage to have a per-user store
        //   note that this doesn't actually secure anything:
        //   it only prevents name conflicts
        self.store = localforage.createInstance({
          name: self.user,
          version: 1.0
        });

        var getProjects =
          self.store.getItem("//projects")
          .then(function(projs) {
            self.projects = projs || [];
            console.log("[localfiles] projects", self.projects);
          });

        var getOfflineChanges =
          self.store.getItem("//offlineChangelog")
          .then(function(data) {
            self.offlineChangelog = [];
            self.offlineChangelogSet = {};
            for (var id in data) {
              var oc = data[id];
              var offlineChange = new OfflineChange(oc._project, oc._path, oc._isDeleted);
              var key = sprintf("%s/%s", offlineChange.getProject(), offlineChange.getPath());
              self.offlineChangelog.push(offlineChange);
              self.offlineChangelogSet[key] = true;
            }
            console.log("offlineChangelog", self.offlineChangelog);
            console.log("offlineChangelogSet", self.offlineChangelogSet);
          });

        var getOfflineDeletedFiles =
          self.store.getItem("//offlineDeletedFiles")
          .then(function(data) {
            data = data || {};
            self.offlineDeletedFiles = {};
            for (var id in data) {
              var oc = data[id];
              var offlineChange = new OfflineChange(oc._project, oc._path, oc._isDeleted);
              var key = sprintf("%s/%s", offlineChange.getProject(), offlineChange.getPath());
              self.offlineDeletedFiles[key] = offlineChange;
            }
          });

        return $q.all([getProjects, getOfflineChanges, getOfflineDeletedFiles])
          .then(function () { return true; });
      };

      /*
       * Returns the path to where this file is stored.
       */
      self._path = function(project, file) {
        return sprintf("%s/%s", project, file);
      };

      /*
       * Save a file to local storage.
       * @param {string} name: project name
       * @param {string} file_name: filename
       * @param {string} file_content: The contents of the file
       * @param {string | false} checksum: MD5 checksum of the contents,
       *   or false for an offline-write
       */
      self.writeFile = function(name, file_name, file_content, checksum) {
        var offline_checksum = md5(file_content);
        var path = self._path(name, file_name);

        return self._getProject(name).then(function(tree) {
          var found = false;
          var prom;
          for(var i=0; i<tree.length; i++) {
            if(tree[i].path==file_name) {
              found = tree[i];
            }
          }
          if(!found) {
            tree.push({path: file_name, online_checksum: checksum,
                offline_checksum: offline_checksum});
          }
          else {
            found.offline_checksum = offline_checksum;
            if(checksum) {
              found.online_checksum = checksum;
            }
          }
          prom = self.store.setItem(sprintf("//projects/%s", name), tree);
          return prom.then(function() {
            console.log("[localfiles] Offline Write", contents);
            return self.store.setItem(path, contents);
          });
        });
      };

      self.batchWrite = function(name, files, contents, checksums) {
        var offline_checksums = _.map(contents, md5);
        var paths = _.map(files, function(file) { return self._path(name, file); });

        return self._getProject(name).then(function(tree) {
          for(var f=0; f<files.length; f++) {
            var found = false;
            for(var i=0; i<tree.length; i++) {
              if(tree[i].path === files[f]) {
                found = tree[i];
              }
            }
            if(!found) {
              tree.push({path: files[f], online_checksum: checksums[f],
                  offline_checksum: offline_checksums[f]});
            }
            else {
              found.offline_checksum = offline_checksums[f];
              if(checksums[f]) {
                found.online_checksum = checksums[f];
              }
            }
          }
          var prom = self.store.setItem(sprintf("//projects/%s", name), tree);
          return prom.then(function() {
            console.log("[localfiles] Offline Batch Write", files);
            return $q.all(_.map(_.zip(paths, contents), function(p) {
              return self.store.setItem(p[0], p[1]);
            }));
          });
        });
      };

      self.readFile = function(name, file_name) {
        return $q.when(self.store.getItem(self._path(name, file_name))).then(
          function(contents) {
            console.log("[localfiles] Reading", contents);
            return contents;
          });
      };


      self.renameFile = function(project, old_name, new_name) {
        self.readFile(project, old_name)
          .then(
            function(contents) {
              self.writeFile(project, new_name, contents.data, contents.online_checksum);
            })
          .then(
            function() {
              self.deleteFile(project, old_name);
            });
      };

      self.deleteFile = function(name, file_name) {
        console.log("[localfiles] deleteFile");
        self._addOfflineDelete(name, file_name);
        return self.store.getItem(sprintf("//projects/%s", name)).then(function(tree) {
          var i = 0;
          var found = false;
          for(; i<tree.length; i++) {
            if(tree[i].path === file_name) {
              found = true;
              break;
            }
          }
          if(found) {
            tree.splice(i, 1);
          }
          return self.store.setItem(sprintf("//projects/%s", name), tree).then(function() {
            return $q.when(self.store.removeItem(self._path(name, file_name))); 
          });
        });
      };

      self.batchDelete = function(name, files) {
        console.log("[localfiles] batchDelete");

        _.each(files, function(f) { self._addOfflineDelete(name, f); });
        return self.store.getItem(sprintf("//projects/%s", name)).then(function(tree) {
          for(var f=0; f<files.length; f++) {
            var i = 0;
            var found = false;
            for(; i<tree.length; i++) {
              if(tree[i].path === files[f]) {
                found = true;
                break;
              }
            }
            if(found) {
              tree.splice(i, 1);
            }
          }
          return self.store.setItem(sprintf("//projects/%s", name), tree).then(function() {
            return $q.all(_.map(files, function(f) {
              return self.store.removeItem(self._path(name, f));
            }));
          });
        });
      };

      self.getRunnerFile = function(name, question) {
        return self.store.getItem(self._path(name, question) + "//runnerFile")
          .then(function(contents) {
            console.log("[localfiles] getRunnerFile", contents);
            return contents;
          });
      };

      self.setRunnerFile = function(name, question, folder, file) {
        if (folder == "common" || folder == "tests")
          return $q.reject("Runner file must be in question directory.");
        console.log("[localfiles] setRunnerFile");
        return $q.when(self.store.setItem(self._path(name, question) + "//runnerFile", file));
      };

      self._getProject = function(name) {
        // return the entire SeashellProject tree
        return self.store.getItem(sprintf("//projects/%s", name))
          .then(function(tree) {
            if(!Array.isArray(tree)) return [];
            return tree;
          });
      };

      self.listProject = function(name) {
        // outward-facing listProject matching the type of result returned from
        //  online listProject
        return self._getProject(name).then(function(files) {
          var result = _.map(files, function(f) {
            return [f.path, false, 0, f.online_checksum];
          });
          /*_.each(files, function(f) {
            var dir = f.path.split("/");
            dir.pop();
            dir = dir.join("/");
            if(result.indexOf(dir)===-1) {
              result.push([dir, true, 0, ""]);
            }
          });*/
          return result;
        });
      };

      self.newDirectory = function(name, dir_path) {
        console.log("[localfiles] newDirectory", name, dir_path);
        // do nothing, since dumpProject will take care of this
      };

      self.newFile = function(name, file_name, contents,
        encoding, normalize) {
        console.log("[localfiles] newFile", name, file_name, contents);
        // TODO: decoding 
        // name: project name
        // file_name: relative path under project
        self.writeFile(name, file_name, contents, false);
      };


      // Overwrite the offline project list with a new list of projects 
      self.setProjects = function(projects) {
        self.projects = projects;
        return $q.when(self.store.setItem("//projects", self.projects));
      };

      self.newProject = function(name) {
        console.log("[localfiles] newProject", name);
        self.projects.push(name);
        return self.store.getItem("//projects").then(function(projects) {
          if(projects.indexOf(name)===-1)
            projects.push(name);
          return self.store.setItem("//projects", self.projects);
        });
      };

      // Assumes all files contained in the project have already been deleted.
      self.deleteProject = function(name) {
        console.log("[localfiles] deleteProject", name);
        return $q.all([self.getProjects(), self.store.getItem(sprintf("//projects/%s", name))])
          .then(function(res) {
            var proms = [];
            var files = res[1];
            var projects = res[0];
            _.each(files, function(f) {
              proms.push(self.deleteFile(name, f[0]));
            });
            proms.push($q.when(self.store.removeItem(sprintf("//projects/%s", name))));
            projects = _.filter(projects, function(p) {
              return p !== name;
            });
            proms.push($q.when(self.store.setItem("//projects", projects)));
            return $q.all(proms);
          });
      };

      self.batchDeleteProjects = function(names) {
        console.log('[localfiles] batchDeleteProjects', names);
        return $q.all(_.map(names, function(p) {
          return {name: p, tree: self.store.getItem(sprintf("//projects/%s", p))};
        })).then(function(names_trees) {
          var proms = [];
          _.each(names_trees, function (name_tree) {
            var name = name_tree.name;
            var tree = name_tree.tree;
              _.each(tree, function(f) {
                proms.push(self.deleteFile(name, f[0]));
              });
              proms.push(self.store.removeItem(sprintf("//projects/%s", name)));
              self.projects = _.filter(self.projects, function(n) {
                return n !== name;
              });
            });
            proms.push(self.store.setItem("//projects", self.projects));
          return $q.all(proms);
        });
      };

      self.getProjects = function() {
        return self.store.getItem("//projects").then(function(proj) {
          if(!Array.isArray(proj)) return [];
          return proj;
        });
      };
    }
  ]);
