import Dexie from 'dexie';
import 'js-cookie';
import md5 = require('md5');
import {sprintf} from 'sprintf-js';
import * as R from 'ramda';
export {Store, File, Project}

// subject to changes
interface ChangeLog {
  type: string,  
  file: string
}

interface DBOptions {
  addons?: Array<(db: Dexie) => void>,
  autoOpen?: boolean,
  indexedDB?: IDBFactory,
  IDBKeyRange?: new () => IDBKeyRange
}

// subject to changes
interface File {
  project: string;
  name: string;
  contents: string;
  history?: string;
  checksum?: string;
  last_modified?: number;
}

// subject to changes
interface Project {
  name: string;
  settings?: any;
  last_modified?: number;
  last_visited?: number;
}

// subject to changes
interface Settings {

}

class Store {

  private db: StorageDB;
  private has_offline_changes: boolean;

  public constructor(dbName: string, options: DBOptions) {
    this.db = new StorageDB(dbName, options);
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
  public async writeFile(proj: string, name: string, contents: string, history: string, checksum: string) {
    var offline_checksum = md5(contents);
    var key = [proj, name];
    await this.db.transaction('rw', this.db.files, async () => {
      if (! checksum) {
        let current: File = await this.db.files.get(key);
          // this.db.changelog.add({
          //   file: {
          //     project: proj, 
          //     name: name, 
          //     checksum: current.checksum
          //   },
          //   type: "editFile",
          //   history: history,
          //   contents: contents
          // });
      }
      await this.db.files.update(key, {
        contents: contents,
        history: history,
        checksum: checksum,
        last_modified: Date.now()
      });
      return offline_checksum;
    });
  }

  public async readFile(proj: string, name: string) {
    return this.db.files.get([proj, name]);
  }

  public async deleteFile(proj: string, name: string, online: boolean) {
    var self = this;
    var key = [proj, name];
    await this.db.transaction('rw', this.db.files, async () => {
      if (online) {
        await this.db.files.delete(key);
      } else {
        let current = await this.db.files.get(key);
          // this.db.changelog.add({
          //   file: {
          //     project: proj, 
          //     file: file, 
          //     checksum: current.checksum
          //   },
          //   type: "deleteFile"
          // });
        // await this.db.files.update(key, {
        //   contents: "test"
        // });
        // console.warn(key);
        await this.db.files.delete(key);
      }
    });
  }

  public renameFile(name: string, file: string, newName: string, checksum: string) {
    var self = this;
    var key = [name, file];
    return self.db.transaction('rw', self.db.files, self.db.changelog, function () {
      return self.db.files.get(key)
        .then(function (result) {
          self.deleteFile(name, file, !!checksum);
          result.name = newName;
          if (checksum !== undefined) {
            self.db.files.add(result);
          } else {
            var change = {
              file:{
                project: name, 
                file: newName
              },
              type: "newFile",
              contents: result.contents,
              history: result.history
            }
            // self.db.changelog.add(change);
            self.db.files.add(result);
          }
          return result.checksum;
        });
    });
  }

  public getFileToRun(name: string, question: string) {
    var self = this;
    return self.db.projects.get(name).then(function (project) {
      return project.settings[question+"_runner_file"];
    });
  }

  public setFileToRun(name: string, question: string, folder: string, file: string) {
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
  }

  // public getSettings(get_all: boolean) {
  //   var self = this;
  //   return self.db.settings.get("settings").then(function(settings) {
  //     if (settings && get_all) {
  //       return settings;
  //     } else if (settings) {
  //       return settings.values;
  //     }
  //     return get_all ? {values:{}, modified: 0, name: "settings"} : {}
  //   });
  // }

  // public saveSettings(settings: Settings) {
  //   var self = this;
  //   return self.db.settings.put({
  //     name: "settings",
  //     values: settings,
  //     modified: (new Date()).getTime()
  //   });
  // }

  public getMostRecentlyUsed(project: string, question: string) {
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
  }

  public updateMostRecentlyUsed(project: string, question: string, file: string) {
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
  }

  public async listProject(name: string) {
    // this is called when we open a project, so we will update the last modified time here as well
    return this.db.transaction('rw', this.db.projects, this.db.files, () => {
      this.db.projects.get(name).then((p: Project) => {
        p.last_modified = Date.now();
        this.db.projects.put(p);
      });
      return this.db.files.where('project').equals(name).toArray((x: File[]) => x);
    });
  }

  public listAllProjectsForSync() {
    var self = this;
    return self.db.files.toArray(function (files) {
      return files.map(function(file) {
        return {
          project: file.project, 
          file: file.name, 
          checksum: file.checksum
        }
      });
    });
  }

  // public newDirectory(name: string, path: string) {
  //   var self = this;
  //   return new Dexie.Promise(function (resolve, reject) {resolve(true);});
  // }

  // public deleteDirectory(name: string, path: string) {
  //   var self = this;
  //   return new Dexie.Promise(function (resolve, reject) {resolve(true);});
  // }

  public async newFile(proj: string, file: string, contents: string, encoding: string, normalize: boolean, online_checksum: string) {
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
    var key = [proj, file];
    return this.db.transaction('rw', this.db.files, function() {
      let f: File = {
        project: proj, 
        name: file,
        contents: contents, 
        history: "", 
        checksum: checksum,
        last_modified: Date.now()
      }
      if (online_checksum) {
        // TODO: Set history when syncing.
        return this.db.files.add(f).then(() => checksum);
      } else {
        // this.db.changelog.add({
        //   file: {
        //     project: proj, 
        //     file: file
        //   },
        //   type: "newFile",
        //   contents: contents
        // });
        this.db.files.add(f);
        return checksum;
      }
    });
  }

  public async newProject(name: string) {
    this.db.projects.add({
      name: name, 
      settings: {}, 
      last_modified: Date.now()
    });
  }

  public async deleteProject(name: string, online: boolean) {
    var self = this;
    this.db.transaction('rw', this.db.files, this.db.projects, async () => {
      this.db.files.where('project').equals(name).each(async (file: File) => {
        await self.deleteFile(name, file.name, online);
        await self.db.projects.delete(name);
      });
    });
  }

  // expects a project object in the form described in collects/seashell/backend/offline.rkt
  public async updateProject(proj: Project) {
    // project.settings = JSON.parse(project.settings);
    return this.db.transaction('rw', this.db.projects, () => {
      this.db.projects.update(proj.name, proj);
    });
  }

  public getProjectsForSync() {
    var self = this;
    return self.db.projects.toArray(function(projects) {
      return projects.map(function(project) {
        project.settings = JSON.stringify(project.settings);
        return project;
      });
    });
  }

  public async getProjects() {
    return this.db.projects.toCollection().toArray((x:Project[]) => x);
  }

  // public applyChanges(changes, newProjects: Project[], deletedProjects: Project[], updatedProjects: Project[], settings: Settings) {
  //   var self = this;
  //   return self.db.transaction('rw', self.db.files, self.db.changelog, self.db.projects, self.db.settings, function() {
  //     Dexie.currentTransaction.on('abort', function(ev) {
  //       console.log("applyChanges transaction aborted", ev);
  //       throw ev.target.error;
  //     });
  //     newProjects.forEach(function (project) {
  //       self.newProject(project);
  //     });
  //     changes.forEach(function (change) {
  //       if (change.type === "deleteFile") {
  //         self.deleteFile(change.file.project, change.file.file, true);
  //       } else if (change.type === "editFile") {
  //         self.writeFile(change.file.project, change.file.file, change.contents, change.history, change.file.checksum);
  //       } else if (change.type === "newFile") {
  //         self.newFile(change.file.project, change.file.file, change.contents, undefined, undefined, change.file.checksum);
  //       } else {
  //         throw sprintf("applyChanges: unknown change %s!", change);
  //       }
  //     });
  //     deletedProjects.forEach(function (project) {
  //       self.deleteProject(project, true);
  //     });
  //     updatedProjects.forEach(function(project) {
  //       self.updateProject(project);
  //     });
  //     if (settings) {
  //       self.saveSettings(JSON.parse(settings));
  //     }
  //     self.db.changelog.clear();
  //   });
  // }

  // public getOfflineChanges() {
  //   var self = this;
  //   return self.db.changelog.toArray();
  // }

  // public hasOfflineChanges = function() {
  //   var self = this;
  //   self.db.changelog.count().then(function(c: number) {
  //     self.has_offline_changes = c > 0;
  //   });
  //   return self.has_offline_changes;
  // }
}


class StorageDB extends Dexie {
  public changelog: Dexie.Table<ChangeLog, number>;
  public files: Dexie.Table<File, string[]>;
  public projects: Dexie.Table<Project, string>;
  public settings: Dexie.Table<Settings, string>;

  public constructor(dbName: string, options?: DBOptions) {
    super(dbName, options);
    this.version(1).stores({
      changelog: '++id',
      files: '[project+name], project',
      projects: 'name',
      settings: 'name'
    });
  }
}
