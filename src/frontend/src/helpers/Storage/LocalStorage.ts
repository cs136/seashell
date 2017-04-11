import Dexie from "dexie";
import md5 = require("md5");
import {sprintf} from "sprintf-js";
import * as R from "ramda";
import {AbstractStorage,
        File, FileID, FileBrief,
        Project, ProjectID, ProjectBrief,
        Settings, defaultSettings} from "./Interface";
import * as E from "../Errors";

export {LocalStorage, ChangeLog}

// it remains the 21th centry's be biggest mistery
// how the backend expects file.file to be the filename
interface ChangeLog {
  id?: number;
  type: "newFile" | "deleteFile" | "editFile";
  contents?: string;
  file: {file: string, project: string};
}

interface DBOptions {
  addons?: Array<(db: Dexie) => void>;
  autoOpen?: boolean;
  indexedDB?: IDBFactory;
  IDBKeyRange?: new () => IDBKeyRange;
}

class LocalStorageError extends Error {
  constructor(e: string) {
    super(e);
  }
}

class LocalStorage implements AbstractStorage {
  [index: string]: any; // supress type errors

  private db: StorageDB;

  public constructor() {}

  public async connect(dbName: string): Promise<void> {
    this.db = new StorageDB(dbName, {
      IDBKeyRange: (<any>window).IDBKeyRange,
      indexedDB: (<any>window).indexedDB
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
  public async writeFile(fid: FileID, contents: string): Promise<void> {
    const checksum = md5(contents);
    const tbs = [this.db.files, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      let file: File = await this.readFile(fid);
      await this.pushChangeLog({
        type: "editFile",
        file: {file: file.name, project: file.project},
        contents: contents
      });
      await this.db.files.update(fid, {
        contents: contents,
        checksum: checksum,
        last_modified: Date.now()
      });
    });
  }

  public async readFile(fid: FileID): Promise<File> {
    const file = await this.db.files.get(fid);
    if (! file) {
      throw new LocalStorageError(`readFile("${fid}"):\nFile "${fid}" does not exist.`);
    }
    return file;
  }

  public async deleteFile(id: FileID): Promise<void> {
    const tbs = [this.db.files, this.db.projects, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      let file = await this.readFile(id);
      await this.db.files.delete(id);
      await this.pushChangeLog({
        type: "deleteFile",
        file: {file: file.name, project: file.project}
      });
      // also remove from run files
      let dbProj = await this.db.projects.get(file.project);
      // when a project is deleted by both frontend abd backend,
      // in the next sync backend still asks the frontend to delete children
      // which no longer exists
      if (! dbProj) {
        console.warn(id, file);
        return;
      }
      for (const q in dbProj.runs) {
        if (R.equals(dbProj.runs[q], id)) {
          delete dbProj.runs[q];
        }
      }
      await this.db.projects.update(file.project, dbProj);
    });
  }

  public async renameFile(fid: FileID, newName: string): Promise<void> {
    const tbs = [this.db.files, this.db.projects, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      const file = await this.readFile(fid);
      await this.db.files.update(fid, {
        name: newName
      });
      this.pushChangeLog({
        type: "newFile",
        contents: file.contents,
        file: {file: newName, project: file.project}
      });
      this.pushChangeLog({
        type: "deleteFile",
        file: {file: file.name, project: file.project}
      });
    });
  }

  public async getFileToRun(proj: ProjectID, question: string): Promise<FileID|false> {
    let p = await this.db.projects.get(proj);
    return p.runs[question] || false;
  }

  // a file name is (test|q*|common)/name
  public async setFileToRun(proj: ProjectID, question: string, file: FileID): Promise<void> {
    return await this.db.transaction("rw", this.db.projects, async () => {
      let current = await this.db.projects.get(proj);
      if (! current) {
        throw new LocalStorageError(`setFileToRun: project ${proj} does not exist.`);
      }
      current.runs[question] = file;
      await this.db.projects.update(proj, current);
    });
  }

  public async getSettings(): Promise<Settings> {
    const settings = await this.db.settings.get(0);
    return settings || defaultSettings;
  }

  public async setSettings(settings: Settings): Promise<void> {
    await this.db.settings.put(settings);
  }

  // public getMostRecentlyUsed(project: string, question: string): Promise<FileID> {
  //   return await this.db.transaction("rw", this.db.projects, function() {
  //     return await this.db.projects.get(project).then(function(current: Project) {
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
  //   return await this.db.transaction("rw", this.db.projects, function() {
  //     this.db.projects.get(project).then(function(current: Project) {
  //       if (question) {
  //         current.settings[question+"_most_recently_used"] = file;
  //       } else {
  //         current.settings.most_recently_used = file;
  //       }
  //       return await this.db.projects.put(current);
  //     });
  //   });
  // }

  public async getProjectFiles(pid: ProjectID): Promise<FileBrief[]> {
    // this is called when we open a project, so we will update the last modified time here as well
    return await this.db.transaction("rw", this.db.projects, this.db.files, async () => {
      const p: Project = await this.getProject(pid);
      p.last_modified = Date.now();
      await this.db.projects.put(p);
      return await this.db.files.where("project").equals(pid).toArray((arr) => {
        // you should remove fields that are too large here if necessary
        for (const item of arr) {
          delete item.contents;
        }
        return arr;
      });
    });
  }

  public async newFile(proj: ProjectID,
                       name: string,
                       contents?: string,
                       base64?: boolean): Promise<FileID> {
    let rmatch: RegExpMatchArray;
    if (base64 && (rmatch = contents.match(/^data:([^;]*)?(?:;(?!base64)([^;]*))?(?:;(base64))?,(.*)/))) {
      const mime = rmatch[1];
      const b64 = rmatch[3];
      if (b64 || mime === "base64") {
        contents = window.atob(rmatch[4]);
      }
    }
    const checksum = md5(contents);
    const tbs = [this.db.files, this.db.projects, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      /*
        Known problem:
          If you rename a file from A to B, then you won't be able to create a file named A again,
          since rename should not change the original id of the file.
        How to fix:
          Make sure the id is unique for each new file. For example: const id = md5(proj + name + Date.now()).
          This requires the backend to be aware of file ids
      */
      const id = md5(proj + name);
      const file = await this.db.files.where({
        name: name,
        project: proj
      });
      if (await file.count() > 0) {
        throw new LocalStorageError(`file "${proj}" "${name}" already exists.`);
      }
      const project = await this.db.projects.get(proj);
      if (! project) {
        throw new LocalStorageError(`Project "${proj}" doesn't exist.`);
      }
      const fid = await this.db.files.add({
        id: id,
        project: proj,
        name: name,
        contents: contents,
        checksum: checksum,
        last_modified: Date.now()
      });
      await this.pushChangeLog({
        type: "newFile",
        contents: contents,
        file: {file: name, project: proj}
      });
      return fid;
    });
  }

  public async newProject(name: string): Promise<ProjectID> {
    await this.db.projects.add({
      id: name,
      name: name,
      runs: {},
      last_modified: Date.now(),
      open_tabs: {}
    });
    return name;
  }

  public async deleteProject(proj: ProjectID): Promise<void> {
    const tbs = [this.db.projects, this.db.files];
    return await this.db.transaction("rw", tbs, async () => {
      await this.db.projects.delete(proj);
      const files = await this.db.files.where("project").equals(proj);
      await files.delete();
    });
  }

  public async getProject(pid: ProjectID): Promise<Project> {
    const p = await this.db.projects.get(pid);
    if (! p) {
      throw new LocalStorageError(`Project ${pid} doesn't exist.`);
    }
    return p;
  }

  public async getProjects(): Promise<ProjectBrief[]> {
    return await this.db.projects.toCollection().toArray();
  }

  public async getAllFiles(): Promise<FileBrief[]> {
    const result = await this.db.files.toArray();
    return R.map((file) => ({
      id: file.id,
      name: file.name,
      project: file.project,
      last_modified: file.last_modified,
      checksum: file.checksum
    }), result);
  }

  public async getOpenTabs(proj: ProjectID, question: string): Promise<FileID[]> {
    return [];
  }

  public async setOpenTabs(proj: ProjectID, question: string, files: FileID[]): Promise<void> {
  }

  public async getChangeLogs(): Promise<ChangeLog[]> {
    return await this.db.changeLogs.orderBy("id").reverse().toArray();
  }

  public async topChangeLog(): Promise<ChangeLog|false> {
    const log = await this.db.changeLogs.orderBy("id").reverse().limit(1).first();
    return log || false;
  }

  public async pushChangeLog(change: ChangeLog): Promise<number> {
    const top = await this.topChangeLog();
    if (top && change.type === "editFile" && top.type === "editFile") {
      await this.popChangeLog();
      top.contents = change.contents;
    }
    return await this.db.changeLogs.put(change);
  }

  public async popChangeLog(): Promise<ChangeLog|false> {
    const top = await this.topChangeLog();
    if (top) {
      await this.db.changeLogs.delete(top.id);
      return top;
    } else {
      return false;
    }
  }

  public async countChangeLogs(): Promise<number> {
    return await this.db.changeLogs.count();
  }

  public async clearChangeLogs(): Promise<void> {
    this.db.changeLogs.clear();
  }

  // Will be replaced by Dexie.Syncable.ISyncProtocol
  public async applyChanges(changeLogs: ChangeLog[],
                            newProjects: string[],
                            deletedProjects: string[]): Promise<void> {
    const tbs = [this.db.files, this.db.changeLogs, this.db.projects, this.db.settings];
    return await this.db.transaction("rw", tbs, async () => {
      Dexie.currentTransaction.on("abort", () => {
        console.warn("applyChanges transaction aborted");
      });
      for (const proj of newProjects) {
        await this.newProject(proj);
      }
      for (const change of changeLogs) {
        const fid = md5(change.file.project + change.file.file);
        if (change.type === "deleteFile") {
          await this.deleteFile(fid);
        } else if (change.type === "editFile") {
          await this.writeFile(fid, change.contents);
        } else if (change.type === "newFile") {
          await this.newFile(change.file.project, change.file.file, change.contents);
        } else {
          throw sprintf("applyChanges: unknown change %s!", change);
        }
      }
      for (const proj of deletedProjects) {
        await this.store.deleteProject(proj, true);
      };
      await this.clearChangeLogs();
    });
  }
}


class StorageDB extends Dexie {
  public changeLogs: Dexie.Table<ChangeLog, number>;
  public files: Dexie.Table<File, FileID>;
  public projects: Dexie.Table<Project, ProjectID>;
  public settings: Dexie.Table<Settings, number>;

  public constructor(dbName: string, options?: DBOptions) {
    super(dbName, options);
    this.version(1).stores({
      changeLogs: "++id",
      files: "id, [name+project], name, project",
      projects: "name",
      settings: "id"
    });
  }
}
