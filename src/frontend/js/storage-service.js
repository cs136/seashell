  /* jslint esversion: 6 */
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
          projects: 'name'
        });
        /*
         * Save a file to local storage.
         * @param {string} name: project name.
         * @param {string} file_name: filename.
         * @param {string} file_content: The contents of the file.
         * @param {string} file_history: The history of the file.
         * @param {string || any false value} checksum : The online checksum of the file, false to not update (offline write).
         */
        self.writeFile = (name, file, contents, history, checksum) => {
          let offline_checksum = md5(contents);
          let key = [name, file];
          return self.database.transaction('rw', self.database.changelog, self.database.files, () => {
            if (checksum !== undefined) {
              return self.database.files.update(key,
                  {contents: contents, history: history, checksum: checksum,
                   last_modified: new Date()})
                .then((result) => {
                  if (!result) {
                    throw sprintf("Storage.writeFile: file %s/%s not found!", name, file);
                  }
                  return offline_checksum;
                });
            } else {
              return self.database.files.get(key)
                .then((current) => {
                  self.database.changelog.add({file:{project: name, file: file, checksum: current.checksum},
                                               type: "editFile",
                                               contents: contents});
                  self.database.files.update(key,
                    {contents: contents, history: history, checksum: offline_checksum,
                     last_modified: new Date()});
                  return offline_checksum;
                });
            }
          });
        };

        self.readFile = (name, file) => {
          let key = [name, file];
          return self.database.files.get(key)
            .then((result) => {
              return {data: result.contents, history: result.history};
            });
        };

        self.deleteFile = (name, file, online) => {
          let key = [name, file];
          return self.database.transaction('rw', self.database.changelog, self.database.files, () => {
            if (online !== undefined) {
              return self.database.files.delete(key);
            } else {
              return self.database.files.get(key)
                .then((current) => {
                  self.database.changelog.add({file:{project: name, file: file, checksum: current.checksum},
                                               type: "deleteFile"});
                  return self.database.files.delete(key);
                });
            }
          });
        };

        self.renameFile = (name, file, new_file, checksum) => {
          let key = [name, file];
          return self.database.transaction('rw', self.database.files, self.database.changelog, () => {
            return self.database.files.get({project: name, file: file})
              .then((result) => {
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

        self.getFileToRun = (name, question) => {
          return self.database.projects.get(name)
            .then((project) => {
              return project.settings.runner_files[question];
            });
        };

        self.setFileToRun = (name, question, folder, file) => {
          if (folder === "tests") {
            throw sprintf("Storage.setFileToRun: folder cannot be tests/!");
          }
          return self.database.transaction('rw', self.database.projects, () => {
            self.database.projects.get(name).then((current) => {
              current.settings.runner_files[question] = folder === "question" ? sprintf("%s/%s", question, file) : sprintf("%s/%s", folder, file);
              return self.database.projects.put(current);
            });
          });
        };

        self.listProject = (name) => {
          return self.database.files.where('project').equals(name).toArray(
          (files) => {
            return files.map(
              (file) => {
                return [file.file, false, file.last_modified.getUTCMilliseconds(), file.checksum];
              });
          });
      };

      self.listAllProjectsForSync = () => {
        return self.database.files.toArray();
      };

      self.newDirectory = (name, path) => {
        return new Dexie.Promise((resolve, reject) => {resolve(true);});
      };

      self.deleteDirectory = (name, path) => {
        return new Dexie.Promise((resolve, reject) => {resolve(true);});
      };

      self.newFile = (name, file, contents, encoding, normalize, online) => {
        let checksum = md5(contents);
        let key = [name, file];
        return self.database.transaction('rw', self.database.changelog, self.database.files, () => {
          if (online !== undefined) {
            return self.database.files.add({project: name, file: file,
                                            contents: contents, history: "",
                                            checksum: checksum,
                                            last_modified: new Date()})
              .then(() => {
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

      self.newProject = (name) => {
        return self.database.projects.add({name: name, settings: {runner_files: {}}, last_modified: new Date()});
      };

      self.deleteProject = (name, online) => {
        return self.database.transaction('rw', self.database.files, self.database.changelog, self.database.projects, () => {
          self.database.files.where('project').equals(name).each((file) => {
            self.deleteFile(name, file.file, online);
          });
          self.database.projects.delete(name);
        });
      };

      self.getProjects = () => {
        return self.database.projects.toCollection().toArray((projects) => {
          return projects.map((project) => {
            return [project.name, project.last_modified.getUTCMilliseconds()];
          });
        });
      };

      self.applyChanges = (changes, newProjects, deletedProjects) => {
        return self.database.transaction('rw', self.database.files, self.database.changelog, self.database.projects, () => {
          // TODO: send back project settings with project
          newProjects.forEach((project) => {
            self.newProject(project);
          });
          changes.forEach((change) => {
            if (change.type === "deleteFile") {
              self.deleteFile(change.file.project, change.file.file, true);
            } else if (change.type === "editFile") {
              self.writeFile(change.file.project, change.file.file, change.contents, change.history, change.file.checksum);
            } else if (change.type === "newFile") {
              self.newFile(change.file.project, change.file.file, change.contents, undefined, undefined, true);
            } else {
              throw sprintf("applyChanges: unknown change %s!", change);
            }
          });
          deletedProjects.forEach((project) => {
            self.deleteProject(name, true);
          });
          self.database.changelog.clear();
        });
      };

      self.getOfflineChanges = () => {
        return self.database.changelog.toArray();
      };
    }
  ]);
