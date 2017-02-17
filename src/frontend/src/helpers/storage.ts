import Dexie from 'dexie';
import 'jquery.cookie';
import {md5} from 'blueimp-md5';
import {sprintf} from 'sprintf-js';

// need to import SEASHELL_CREDS_COOKIE
var SEASHELL_CREDS_COOKIE = "";
// need to import SEASHELL_CREDS_COOKIE
var USERNAME = "";

class StorageDB extends Dexie {
  public changelog: Dexie.Table<ChangeLog, number>;
  public files: Dexie.Table<File, string[]>;
  public projects: Dexie.Table<Project, string>;
  public settings: Dexie.Table<Settings, string>;

  public constructor(location: string) {
    super(location);
    self.version(1).stores({
      changelog: '++id',
      files: '[project+file], project',
      projects: 'name',
      settings: 'name'
    });
  }
}

// subject to changes
interface ChangeLog {
}

// subject to changes
interface File {
  project: string;
  name: string;
  contents: string;
  history: string;
  checksum: string;
  lastModified: Date;
}

// subject to changes
interface Project {
  settings: any;
  name: string;
}

// subject to changes
interface Settings {

}

export class Storage {

  private db: StorageDB;
  private username: string;
  private has_offline_changes: boolean;

  public constructor() {
    this.db = new StorageDB(USERNAME + "/seashell-local-files");

    this.username = $.cookie(SEASHELL_CREDS_COOKIE).user;
    this.has_offline_changes = false;
  }

  /*
    * Save a file to local storage.
    * @param {string} name: project name.
    * @param {string} file_name: filename.
    * @param {string} file_content: The contents of the file.
    * @param {string} file_history: The history of the file.
    * @param {string || any false value} checksum : The online checksum of the file, false to not update (offline write).
    */
  writeFile(file: string, contents: string, history: string, checksum: string) {
    var self = this;
    var offline_checksum = md5(contents);
    var key = [name, file];
    return self.db.transaction('rw', self.db.changelog, self.db.files, function () {
      if (checksum !== undefined) {
        return self.db.files.update(key, {
          contents: contents, 
          history: history, 
          checksum: checksum,
          last_modified: Date.now()
        }).then(function (result) {
          if (! result) {
            throw sprintf("Storage.writeFile: file %s/%s not found!", name, file);
          }
          return offline_checksum;
        });
      } else {
        return self.db.files.get(key).then(function (current) {
          self.db.changelog.add({
            file: {
              project: name, 
              file: file, 
              checksum: current.checksum
            },
            type: "editFile",
            history: history,
            contents: contents
          });
          self.db.files.update(key, {
            contents: contents, 
            history: history, 
            checksum: offline_checksum,
            last_modified: Date.now()
          });
          return offline_checksum;
        });
      }
    });
  };

  readFile(name: string, file: string) {
    var self = this;
    var key = [name, file];
    return self.db.files.get(key).then(function (result) {
      return {
        data: result.contents, 
        history: result.history
      };
    });
  };

  deleteFile(name: string, file: string, online: boolean) {
    var self = this;
    var key = [name, file];
    return self.db.transaction('rw', self.db.changelog, self.db.files, function () {
      if (online !== undefined) {
        return self.db.files.delete(key);
      } else {
        return self.db.files.get(key).then(function (current) {
          self.db.changelog.add({
            file: {
              project: name, 
              file: file, 
              checksum: current.checksum
            },
            type: "deleteFile"
          });
          return self.db.files.delete(key);
        });
      }
    });
  };

  renameFile(name: string, file: string, new_file, checksum: string) {
    var self = this;
    var key = [name, file];
    return self.db.transaction('rw', self.db.files, self.db.changelog, function () {
      return self.db.files.get(key)
        .then(function (result) {
          self.deleteFile(name, file, checksum);
          result.file = new_file;
          if (checksum !== undefined) {
            self.db.files.add(result);
          } else {
            var change = {
              file:{
                project: name, 
                file: new_file
              },
              type: "newFile",
              contents: result.contents,
              history: result.history
            };
            self.db.changelog.add(change);
            self.db.files.add(result);
          }
          return result.checksum;
        });
    });
  };

  getFileToRun(name: string, question: string) {
    var self = this;
    return self.db.projects.get(name).then(function (project) {
      return project.settings[question+"_runner_file"];
    });
  };

  setFileToRun(name: string, question: string, folder: string, file: string) {
    var self = this;
    if (folder === "tests") {
      throw sprintf("Storage.setFileToRun: folder cannot be tests/!");
    }
    return self.db.transaction('rw', self.db.projects, function () {
      self.db.projects.get(name).then(function (current) {
        current.settings[question+"_runner_file"] = folder === "question" ? sprintf("%s/%s", question, file) : sprintf("%s/%s", folder, file);
        return self.db.projects.put(current);
      });
    });
  };

  getSettings(get_all: boolean) {
    var self = this;
    return self.db.settings.get("settings").then(function(settings) {
      if (settings && get_all) {
        return settings;
      } else if (settings) {
        return settings.values;
      }
      return get_all ? {values:{}, modified: 0, name: "settings"} : {};
    });
  };

  saveSettings(settings: Settings) {
    var self = this;
    return self.db.settings.put({
      name: "settings",
      values: settings,
      modified: (new Date()).getTime()
    });
  };

  getMostRecentlyUsed(project: string, question: string) {
    var self = this;
    return self.db.transaction('rw', self.db.projects, function() {
      return self.db.projects.get(project).then(function(current: Project) {
        if (question) {
          if (current.settings[question+"_most_recently_used"])
            return current.settings[question+"_most_recently_used"];
          return false;
        }
        return current.settings.most_recently_used ? current.settings.most_recently_used : false;
      });
    });
  };

  updateMostRecentlyUsed(project: string, question: string, file: string) {
    var self = this;
    return self.db.transaction('rw', self.db.projects, function() {
      self.db.projects.get(project).then(function(current: Project) {
        if (question) {
          current.settings[question+"_most_recently_used"] = file;
        } else {
          current.settings.most_recently_used = file;
        }
        return self.db.projects.put(current);
      });
    });
  };

  listProject(name: string) {
    var self = this;
    // this is called when we open a project, so we will update the last modified time here as well
    return self.db.transaction('rw', self.db.projects, self.db.files, function() {
      self.db.projects.get(name).then(function(current) {
        current.last_modified = Date.now();
        return self.db.projects.put(current);
      });
      return self.db.files.where('project').equals(name).toArray(
        function (files) {
          return files.map(
            function (file) {
              return [file.file, false, file.last_modified, file.checksum];
            });
        });
    });
  };

  listAllProjectsForSync() {
    var self = this;
    return self.db.files.toArray(function (files) {
      return files.map(function(file) {
        return {
          project: file.project, 
          file: file.file, 
          checksum: file.checksum
        };
      });
    });
  };

  newDirectory(name: string, path: string) {
    var self = this;
    return new Dexie.Promise(function (resolve, reject) {resolve(true);});
  };

  deleteDirectory(name: string, path: string) {
    var self = this;
    return new Dexie.Promise(function (resolve, reject) {resolve(true);});
  };

  newFile(name: string, file: string, contents: string, encoding: string, normalize: boolean, online_checksum: string) {
    var self = this;
    // account for base64 encoding
    var rmatch;
    if (encoding == "url" &&
        (rmatch = contents.match(/^data:([^;]*)?(?:;(?!base64)([^;]*))?(?:;(base64))?,(.*)/))) {
      var mime = rmatch[1];
      var b64 = rmatch[3];
      if(b64 || mime=="base64") {
        contents = window.atob(rmatch[4]);
      }
    }
    var checksum = (typeof contents === "string" && md5(contents)) || online_checksum || "";
    var key = [name, file];
    return self.db.transaction('rw', self.db.changelog, self.db.files, function () {
      if (online_checksum !== undefined) {
        // TODO: Set history when syncing.
        return self.db.files.add({
          project: name, 
          file: file,
          contents: contents, history: "",
          checksum: checksum,
          last_modified: Date.now()
        }).then(function () {
            return checksum;
        });
      } else {
        self.db.changelog.add({file: {project: name, file: file},
                                      type: "newFile",
                                      contents: contents});
        self.db.files.add({project: name, file: file,
                        contents: contents, history: "", checksum: checksum,
                        last_modified: Date.now()});
        return checksum;
      }
    });
  };

  newProject(name: string) {
    var self = this;
    return self.db.projects.add({name: name, settings: {}, last_modified: Date.now()});
  };

  deleteProject(name: string, online: string) {
    var self = this;
    return self.db.transaction('rw', self.db.files, self.db.changelog, self.db.projects, function () {
      self.db.files.where('project').equals(name).each(function (file) {
        self.deleteFile(name, file.file, online);
      });
      self.db.projects.delete(name);
    });
  };

  // expects a project object in the form described in collects/seashell/backend/offline.rkt
  updateProject(project: Project) {
    var self = this;
    project.settings = JSON.parse(project.settings);
    return self.db.transaction('rw', self.db.projects, function() {
      self.db.projects.put(project);
    });
  };

  getProjectsForSync() {
    var self = this;
    return self.db.projects.toArray(function(projects) {
      return projects.map(function(project) {
        project.settings = JSON.stringify(project.settings);
        return project;
      });
    });
  };

  getProjects() {
    var self = this;
    return self.db.projects.toCollection().toArray(function (projects) {
      return projects.map(function (project) {
        return [project.name, project.last_modified];
      });
    });
  };

  applyChanges(changes, newProjects: Project[], deletedProjects: Project[], updatedProjects: Project[], settings: Settings) {
    var self = this;
    return self.db.transaction('rw', self.db.files, self.db.changelog, self.db.projects, self.db.settings, function() {
      Dexie.currentTransaction.on('abort', function(ev) {
        console.log("applyChanges transaction aborted", ev);
        throw ev.target.error;
      });
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
        self.deleteProject(project, true);
      });
      updatedProjects.forEach(function(project) {
        self.updateProject(project);
      });
      if (settings) {
        self.saveSettings(JSON.parse(settings));
      }
      self.db.changelog.clear();
    });
  };

  getOfflineChanges() {
    var self = this;
    return self.db.changelog.toArray();
  };

  hasOfflineChanges = function() {
    var self = this;
    self.db.changelog.count().then(function(c: number) {
        self.has_offline_changes = c > 0;
      });
      return self.has_offline_changes;
    };
  }
}
