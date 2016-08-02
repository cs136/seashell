/*
 * Angular bindings for the Seashell WebSocket client.
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
 angular.module('seashell-local-files', [])
    .service('localfiles', ['$q', '$cookies',
      function($q, $cookies) {
        "use strict";
        // TODO: per user database.
        var self = this;
        self.database = new Dexie("seashell-local-files");
        self.database.version(1).stores({
          changelog: '++id',
          files: '[project+file], project',
          projects: 'name',
          settings: 'name'
        });
        /*
         * Save a file to local storage.
         * @param {string} name: project name.
         * @param {string} file_name: filename.
         * @param {string} file_content: The contents of the file.
         * @param {string} file_history: The history of the file.
         * @param {string || any false value} checksum : The online checksum of the file, false to not update (offline write).
         */
        self.writeFile = function (name, file, contents, history, checksum) {
          var offline_checksum = md5(contents);
          var key = [name, file];
          return self.database.transaction('rw', self.database.changelog, self.database.files, function () {
            if (checksum !== undefined) {
              return self.database.files.update(key,
                  {contents: contents, history: history, checksum: checksum,
                   last_modified: new Date()})
                .then(function (result) {
                  if (!result) {
                    throw sprintf("Storage.writeFile: file %s/%s not found!", name, file);
                  }
                  return offline_checksum;
                });
            } else {
              return self.database.files.get(key)
                .then(function (current) {
                  self.database.changelog.add({file:{project: name, file: file, checksum: current.checksum},
                                               type: "editFile",
                                               history: history,
                                               contents: contents});
                  self.database.files.update(key,
                    {contents: contents, history: history, checksum: offline_checksum,
                     last_modified: new Date()});
                  return offline_checksum;
                });
            }
          });
        };

        self.readFile = function (name, file) {
          var key = [name, file];
          return self.database.files.get(key)
            .then(function (result) {
              return {data: result.contents, history: result.history};
            });
        };

        self.deleteFile = function (name, file, online) {
          var key = [name, file];
          return self.database.transaction('rw', self.database.changelog, self.database.files, function () {
            if (online !== undefined) {
              return self.database.files.delete(key);
            } else {
              return self.database.files.get(key)
                .then(function (current) {
                  self.database.changelog.add({file:{project: name, file: file, checksum: current.checksum},
                                               type: "deleteFile"});
                  return self.database.files.delete(key);
                });
            }
          });
        };

        self.renameFile = function (name, file, new_file, checksum) {
          var key = [name, file];
          return self.database.transaction('rw', self.database.files, self.database.changelog, function () {
            return self.database.files.get(key)
              .then(function (result) {
                self.deleteFile(name, file, checksum);
                result.file = new_file;
                if (checksum !== undefined) {
                  self.database.files.add(result);
                } else {
                  self.database.changelog.add({file:{project: name, file: new_file},
                                               type: "editFile",
                                               contents: result.contents, history: result.history});
                  self.database.files.add(result);
                }
                return result.checksum;
              });
          });
        };

        self.getFileToRun = function (name, question) {
          return self.database.projects.get(name)
            .then(function (project) {
              return project.settings.runner_files[question];
            });
        };

        self.setFileToRun = function (name, question, folder, file) {
          if (folder === "tests") {
            throw sprintf("Storage.setFileToRun: folder cannot be tests/!");
          }
          return self.database.transaction('rw', self.database.projects, function () {
            self.database.projects.get(name).then(function (current) {
              current.settings.runner_files[question] = folder === "question" ? sprintf("%s/%s", question, file) : sprintf("%s/%s", folder, file);
              return self.database.projects.put(current);
            });
          });
        };

        self.getSettings = function() {
          return self.database.settings.get("settings").then(function(settings) {
            if(settings)
              return settings.values;
            return {};
          });
        };

        self.saveSettings = function(settings) {
          var data = {
            name: "settings",
            values: settings,
            modified: (new Date()).getTime()
          };
          return self.database.settings.add(data);
        };

        self.getMostRecentlyUsed = function(project, dir) {
          return self.database.transaction('rw', self.database.projects, function() {
            self.database.projects.get(project).then(function(current) {
              if(dir) {
                return current.settings.most_recently_used[dir];
              }
              return current.settings.most_recently_used_dir;
            });
          });
        };

        self.updateMostRecentlyUsed = function(project, dir, pred, data) {
          return self.database.transaction('rw', self.database.projects, function() {
            self.database.projects.get(project).then(function(current) {
              if(dir) {
                current.settings.most_recently_used[dir] = data;
              }
              else {
                current.settings.most_recently_used_dir = data;
              }
              return self.database.projects.put(current);
            });
          });
        };

        self.listProject = function (name) {
          return self.database.files.where('project').equals(name).toArray(
          function (files) {
            return files.map(
              function (file) {
                return [file.file, false, file.last_modified.getUTCMilliseconds(), file.checksum];
              });
          });
      };

      self.listAllProjectsForSync = function () {
        return self.database.files.toArray(
        function (files) {
          return files.map(
            function (file) {
              return {project: file.project, file:file.file, checksum: file.checksum};
            });
        });
      };

      self.newDirectory = function (name, path) {
        return new Dexie.Promise(function (resolve, reject) {resolve(true);});
      };

      self.deleteDirectory = function (name, path) {
        return new Dexie.Promise(function (resolve, reject) {resolve(true);});
      };

      self.newFile = function (name, file, contents, encoding, normalize, online_checksum) {
        var checksum = (typeof contents === "string" && md5(contents)) || online_checksum || "";
        var key = [name, file];
        return self.database.transaction('rw', self.database.changelog, self.database.files, function () {
          if (online_checksum !== undefined) {
            // TODO: Set history when syncing.
            return self.database.files.add({project: name, file: file,
                                            contents: contents, history: "",
                                            checksum: checksum,
                                            last_modified: new Date()})
              .then(function () {
                return checksum;
              });
          } else {
            self.database.changelog.add({file: {project: name, file: file},
                                         type: "editFile",
                                         contents: contents});
            self.database.files.add({project: name, file: file,
                            contents: contents, history: "", checksum: checksum,
                            last_modified: new Date()});
            return checksum;
          }
        });
      };

      self.newProject = function (name) {
        return self.database.projects.add({name: name, settings: {runner_files: {}}, last_modified: new Date()});
      };

      self.deleteProject = function (name, online) {
        return self.database.transaction('rw', self.database.files, self.database.changelog, self.database.projects, function () {
          self.database.files.where('project').equals(name).each(function (file) {
            self.deleteFile(name, file.file, online);
          });
          self.database.projects.delete(name);
        });
      };

      self.getProjects = function () {
        return self.database.projects.toCollection().toArray(function (projects) {
          return projects.map(function (project) {
            return [project.name, project.last_modified.getUTCMilliseconds()];
          });
        });
      };

      self.applyChanges = function (changes, newProjects, deletedProjects) {
        return self.database.transaction('rw', self.database.files, self.database.changelog, self.database.projects, function () {
          // TODO: how to sync project settings?  Probably with last-modified-time...
          // TODO: send back project settings with project
          newProjects.forEach(function (project) {
            self.newProject(project);
          });
          changes.forEach(function (change) {
            if (change.type === "deleteFile") {
              self.deleteFile(change.file.project, change.file.file, true);
            } else if (change.type === "editFile") {
              self.writeFile(change.file.project, change.file.file, change.contents, change.history, change.file.checksum);
            } else if (change.type === "newFile") {
              self.newFile(change.file.project, change.file.file, change.contents, undefined, undefined, change.file.checksum);
            } else {
              throw sprintf("applyChanges: unknown change %s!", change);
            }
          });
          deletedProjects.forEach(function (project) {
            self.deleteProject(name, true);
          });
          self.database.changelog.clear();
        });
      };

      self.getOfflineChanges = function () {
        return self.database.changelog.toArray();
      };
    }
  ]);
