import Dexie from 'dexie';
import md5 = require('md5');
import {sprintf} from 'sprintf-js';
import * as R from 'ramda';
export {Storage, 
        File, 
        Project, 
        Settings, 
        StorageInterface}

// Will be replaced by Dexie.Syncable.ISyncProtocol
interface ChangeLog {
  id?: number;
  type: string;
  [index:string]: any;
}

interface DBOptions {
  addons?: Array<(db: Dexie) => void>,
  autoOpen?: boolean,
  indexedDB?: IDBFactory,
  IDBKeyRange?: new () => IDBKeyRange
}

type FileID = [string, string]; // compound key 
interface File {
  id?: FileID;
  name: string; // a file name is (test|q*|common)/name
  project: ProjectID;
  contents: string;
  history?: string;
  checksum?: string;
  last_modified?: number;
}

type ProjectID = string; // alias of name for now
interface Project {
  id?: ProjectID;
  name: string;
  settings?: Settings;
  last_modified?: number;
  runs?: {[index: string]: string}
}

// subject to changes
interface Settings {
  id?: number;
  font_size: number;
  theme: string;
}


interface StorageInterface {

  // Will be replaced by Dexie.Syncable.ISyncProtocol
  getProjectsForSync(): Promise<Project[]>;
  applyChanges(changes: ChangeLog[], newProjects: Project[], deletedProjects: Project[], updatedProjects: Project[], settings: Settings): Promise<void>;
  getOfflineChanges(): Promise<ChangeLog[]>;
  hasOfflineChanges(): Promise<boolean>;

  writeFile(proj: string, name: string, contents: string, history: string, checksum: string): Promise<string>;
  readFile(proj: string, name: string): Promise<File>;
  writeFile(proj: string, name: string, contents: string, history: string, checksum: string): Promise<string>;
  readFile(proj: string, name: string): Promise<File>;
  deleteFile(proj: string, name: string, online: boolean): Promise<void>;
  renameFile(proj: string, name: string, newName: string, checksum: string): Promise<string>;
  getFileToRun(proj: string, question: string): Promise<string|undefined>;
  setFileToRun(proj: string, question: string, directory: string, file: string): Promise<void>;
  getSettings(): Promise<Settings|undefined>;
  saveSettings(settings: Settings): Promise<void>;
  listProject(name: string): Promise<File[]>;
  newFile(proj: string, file: string, contents: string, encoding: string, normalize: boolean, online_checksum: string): Promise<string>;
  newProject(name: string): Promise<void>;
  deleteProject(name: string, online: boolean): Promise<void>;
  updateProject(proj: Project): Promise<void>;
  getProjects(): Promise<Project[]>;
}


class Storage implements StorageInterface {
  [index: string]: any; // supress type errors

  private db: StorageDB;
  private has_offline_changes: boolean;

  public constructor(dbName: string, options: DBOptions) {
    this.db = new StorageDB(dbName, options);
    this.has_offline_changes = false;
  }


  // Will be replaced by Dexie.Syncable.ISyncProtocol
  public async getProjectsForSync(): Promise<Project[]> {
    return this.db.projects.toArray();
    // return this.db.projects.toArray(function(projects) {
    //   return projects.map(function(proj) {
    //     proj.settings = JSON.stringify(project.settings);
    //     return project;
    //   });
    // });
  }

  // Will be replaced by Dexie.Syncable.ISyncProtocol
  public async getOfflineChanges(): Promise<ChangeLog[]> {
    return this.db.changelog.toArray();
  }

  // Will be replaced by Dexie.Syncable.ISyncProtocol
  public async hasOfflineChanges(): Promise<boolean> {
    return this.db.changelog.count().then(function(c: number) {
      return c > 0;
    });
  }

  // Will be replaced by Dexie.Syncable.ISyncProtocol
  public async applyChanges(changes: ChangeLog[], newProjects: Project[], deletedProjects: Project[], updatedProjects: Project[], settings: Settings): Promise<void> {
    const tbs = [this.db.files, this.db.changelog, this.db.projects, this.db.settings];
    return this.db.transaction('rw', tbs, function() {
      Dexie.currentTransaction.on('abort', function() {
        console.log("applyChanges transaction aborted");
      });
      newProjects.forEach(function (project) {
        this.newProject(project);
      });
      changes.forEach(function (change) {
        if (change.type === "deleteFile") {
          this.deleteFile(change["file"].project, change["file"].file, true);
        } else if (change.type === "editFile") {
          this.writeFile(change["file"].project, change["file"].file, change["contents"], change["history"], change["file"].checksum);
        } else if (change.type === "newFile") {
          this.newFile(change["file"].project, change["file"].file, change["contents"], undefined, undefined, change["file"].checksum);
        } else {
          throw sprintf("applyChanges: unknown change %s!", change);
        }
      });
      deletedProjects.forEach(function (project) {
        this.deleteProject(project, true);
      });
      updatedProjects.forEach(function(project) {
        this.updateProject(project);
      });
      if (settings) {
        this.saveSettings(settings);
      }
      this.db.changelog.clear();
    });
  }

  /*
    * Save a file to local storage.
    * @param {string} name: project name.
    * @param {string} file_name: filename.
    * @param {string} file_content: The contents of the file.
    * @param {string} file_history: The history of the file.
    * @param {string || any false value} checksum : The online checksum of the file, false to not update (offline write).
    */
  public async writeFile(proj: string, name: string, contents: string, history: string, checksum: string): Promise<string> {
    const offline_checksum = md5(contents);
    const key: FileID = [proj, name];
    const tbs = [this.db.files, this.db.changelog];
    return this.db.transaction('rw', tbs, async () => {
      if (! checksum) {
        let current: File = await this.db.files.get(key);
          await this.db.changelog.add({
            file: {
              project: proj, 
              name: name, 
              checksum: current.checksum
            },
            type: "editFile",
            history: history,
            contents: contents
          });
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

  public async readFile(proj: string, name: string): Promise<File> {
    return this.db.files.get([proj, name]);
  }

  public async deleteFile(proj: string, name: string, online: boolean): Promise<void> {
    const key: FileID = [proj, name];
    const tbs = [this.db.files, this.db.projects, this.db.changelog];
    return this.db.transaction('rw', tbs, async () => {
      if (! online) {
        let current = await this.db.files.get(key);
        await this.db.changelog.add({
          file: {
            project: proj, 
            file: name, 
            checksum: current.checksum
          },
          type: "deleteFile"
        });
      }
      await this.db.files.delete(key);
      // also remove from run files
      let dbProj = await this.db.projects.get(proj);
      for (const q in dbProj.runs) {
        if (dbProj.runs[q] == name) {
          delete dbProj.runs[q];
        }
      }
      await this.db.projects.update(proj, dbProj)
    });
  }

  public async renameFile(proj: string, name: string, newName: string, checksum: string): Promise<string> {
    const key: FileID = [proj, name];
    const tbs = [this.db.files, this.db.projects, this.db.changelog];
    return this.db.transaction('rw', tbs, async () => {
      let result = await this.db.files.get(key);
      result.name = newName;
      await this.db.files.add(result);
      // also rename in run files
      let dbProj = await this.db.projects.get(proj);
      for (const q in dbProj.runs) {
        if (dbProj.runs[q] == name) {
          dbProj.runs[q] = newName;
        }
      }
      await this.db.projects.update(proj, dbProj)
      if (! checksum) {
        var change = {
          file:{
            project: proj, 
            name: newName
          },
          type: "newFile",
          contents: result.contents,
          history: result.history
        }
        await this.db.changelog.add(change);
      }
      await this.deleteFile(proj, name, !!checksum);
      return result.checksum;
    });
  }

  public async getFileToRun(proj: string, question: string): Promise<string|undefined> {
    let p = await this.db.projects.get(proj);
    return p.runs[question];
  }

  // a file name is (test|q*|common)/name
  public async setFileToRun(proj: string, question: string, directory: string, file: string): Promise<void> {
    return this.db.transaction('rw', this.db.projects, async () => {
      let current = await this.db.projects.get(proj);
      current.runs[question] = file;
      await this.db.projects.update(proj, current);
    })
  }

  public async getSettings(): Promise<Settings|undefined> {
    return this.db.settings.get(0);
  }

  public async saveSettings(settings: Settings): Promise<void> {
    settings.id = 0;
    await this.db.settings.put(settings);
  }

  // public getMostRecentlyUsed(project: string, question: string): Promise<FileID> {
  //   return this.db.transaction('rw', this.db.projects, function() {
  //     return this.db.projects.get(project).then(function(current: Project) {
  //       if (question) {
  //         if (current.settings[question+"_most_recently_used"])
  //           return current.settings[question+"_most_recently_used"];
  //         return false;
  //       }
  //       return current.settings.most_recently_used ? current.settings.most_recently_used : false;
  //     });
  //   });
  // }

  // public updateMostRecentlyUsed(project: string, question: string, file: string): Promise<FileID> {
  //   return this.db.transaction('rw', this.db.projects, function() {
  //     this.db.projects.get(project).then(function(current: Project) {
  //       if (question) {
  //         current.settings[question+"_most_recently_used"] = file;
  //       } else {
  //         current.settings.most_recently_used = file;
  //       }
  //       return this.db.projects.put(current);
  //     });
  //   });
  // }

  public async listProject(name: string): Promise<File[]> {
    // this is called when we open a project, so we will update the last modified time here as well
    return this.db.transaction('rw', this.db.projects, this.db.files, async () => {
      const p: Project = await this.db.projects.get(name);
      p.last_modified = Date.now();
      await this.db.projects.put(p);
      return this.db.files.where('project').equals(name).toArray(R.identity);
    });
  }

  public listAllProjectsForSync() {
    return this.db.files.toArray(function (files) {
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
  //   return new Dexie.Promise(function (resolve, reject) {resolve(true);});
  // }

  // public deleteDirectory(name: string, path: string) {
  //   return new Dexie.Promise(function (resolve, reject) {resolve(true);});
  // }

  public async newFile(proj: string, 
                       file: string, 
                       contents: string, 
                       encoding: string, 
                       normalize: boolean, 
                       online_checksum: string): Promise<string> {
    // account for base64 encoding
    let rmatch: RegExpMatchArray;
    if (encoding == "url" &&
        (rmatch = contents.match(/^data:([^;]*)?(?:;(?!base64)([^;]*))?(?:;(base64))?,(.*)/))) {
      const mime = rmatch[1];
      const b64 = rmatch[3];
      if(b64 || mime=="base64") {
        contents = window.atob(rmatch[4]);
      }
    }
    const checksum = (typeof contents === "string" && md5(contents)) || online_checksum || "";
    const key = [proj, file];
    const tbs = [this.db.files, this.db.changelog];
    return this.db.transaction('rw', tbs, async () => {
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
      }
      await this.db.changelog.add({
        file: {
          project: proj, 
          file: file
        },
        type: "newFile",
        contents: contents
      });
      await this.db.files.add(f);
      return checksum;
    });
  }

  public async newProject(name: string): Promise<void> {
    await this.db.projects.add({
      name: name, 
      runs: {}, 
      last_modified: Date.now()
    });
  }

  public async deleteProject(name: string, online: boolean): Promise<void> {
    const tbs = [this.db.files, this.db.projects, this.db.changelog];
    return this.db.transaction('rw', tbs, async () => {
      const files = await this.db.files.where('project').equals(name).toArray(); 
      for (const file of files) {
        await this.deleteFile(name, file.name, online);
      };
      await this.db.projects.delete(name);
    });
  }

  // expects a project object in the form described in collects/seashell/backend/offline.rkt
  public async updateProject(proj: Project): Promise<void> {
    // project.settings = JSON.parse(project.settings);
    return this.db.transaction('rw', this.db.projects, async () => {
      await this.db.projects.update(proj.name, proj);
    });
  }

  public async getProjects(): Promise<Project[]> {
    return this.db.projects.toCollection().toArray();
  }

}


class StorageDB extends Dexie {
  public changelog: Dexie.Table<ChangeLog, number>;
  public files: Dexie.Table<File, FileID>;
  public projects: Dexie.Table<Project, ProjectID>;
  public settings: Dexie.Table<Settings, number>;

  public constructor(dbName: string, options?: DBOptions) {
    super(dbName, options);
    this.version(1).stores({
      changelog: '++id',
      files: '[project+name], project',
      projects: 'name',
      settings: 'id'
    });
  }
}
