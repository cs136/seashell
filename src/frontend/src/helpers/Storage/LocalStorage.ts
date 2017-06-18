import Dexie from "dexie";
import md5 = require("md5");
import {sprintf} from "sprintf-js";
import * as R from "ramda";
import {AbstractStorage,
        File, FileID, FileBrief, FileStored,
        Project, ProjectID, ProjectBrief, ProjectStored,
        Settings, SettingsStored} from "./Interface";
import * as E from "../Errors";

export {LocalStorage, ChangeLog}


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
  // [index: string]: any; // supress type errors

  private db: StorageDB;
  private dbName: string;

  public constructor(public debug = false) { }

  private setDirty() {
    localStorage.setItem(`${this.dbName}-offline-store-dirty?`, "true");
  }

  private clearDirty() {
    localStorage.setItem(`${this.dbName}-offline-store-dirty?`, "false");
  }

  public async getDirty() {
    return localStorage.getItem(`${this.dbName}-offline-store-dirty?`) === "true"
      || await this.countChangeLogs() > 0;
  }

  public async connect(dbName: string): Promise<void> {
    this.dbName = dbName;
    this.db = new StorageDB(dbName, {
      IDBKeyRange: (<any>window).IDBKeyRange,
      indexedDB: (<any>window).indexedDB
    });

    await this.db.open();
  }

  public async deleteDB(): Promise<void> {
    return this.db.delete();
  }

  // just for now make sure things are consistent
  // you might want to change this later
  public projID(projName: string): ProjectID {
    return md5(projName);
  }

  public fileID(pid: ProjectID, fileName: string): ProjectID {
    return md5(pid + fileName);
  }

  public async writeFile(fid: FileID,
                         contents: string|undefined,
                         checksum?: string,
                         // set pushChangeLog = false in applyChanges to make syncAll faster
                         pushChangeLog: boolean = true): Promise<void> {
    this.debug && console.log(`writeFile`);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      let file: File = await this.readFile(fid);
      const proj: ProjectBrief = await this.getProject(file.project);
      // use the last syncAll checksum
      checksum = checksum || file.checksum;
      if (pushChangeLog) {
        await this.pushChangeLog({
          type: "editFile",
          file: {file: file.name, project: proj.name},
          contents: contents,
          checksum: checksum
        });
      }
      await this.db.files.update(fid, {
        contents: contents,
        checksum: checksum,
        last_modified: Date.now()
      });
    });
  }

  public async readFile(fid: FileID): Promise<File> {
    this.debug && console.log(`readFile`);
    const tbs = [this.db.files];
    return await this.db.transaction("rw", tbs, async () => {
      const file = await this.db.files.get(fid);
      if (! file) {
        throw new LocalStorageError(`file "${fid}" does not exist`);
      }
      return new File(file);
    });
  }

  public async deleteFile(id: FileID,
                          // set pushChangeLog = false in applyChanges to make syncAll faster
                          pushChangeLog: boolean = true): Promise<void> {
    this.debug && console.log(`deleteFile`);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      let file = await this.readFile(id);
      await this.db.files.delete(id);
      const proj: ProjectBrief = await this.getProject(file.project);
      if (pushChangeLog) {
        await this.pushChangeLog({
          type: "deleteFile",
          file: {file: file.name, project: proj.name},
          checksum: file.checksum
        });
      }
      // also remove from run files
      let dbProj = await this.getProject(file.project);
      // when a project is deleted by both frontend and backend,
      // in the next sync backend still asks the frontend to delete children
      // which no longer exists
      // if (! dbProj) {
      //   console.warn(id, file);
      //   return;
      // }
      if (dbProj.settings)
        for (const q in dbProj.settings) {
          if (dbProj.settings[q] === file.name) {
            delete dbProj.settings[q];
          }
        }
      await this.db.projects.update(file.project, dbProj);
    });
  }

  public async renameFile(fid: FileID, newName: string): Promise<FileBrief> {
    this.debug && console.log(`renameFile`);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      const file = await this.readFile(fid);
      const newFile = await this.newFile(file.project, newName, file.contents);
      await this.deleteFile(fid);
      return new FileBrief(newFile);
    });
  }

  private async getProjectSetting(pid: ProjectID, key: string): Promise<string|undefined> {
    return this.db.transaction("r", this.db.projects, async () => {
      const project = await this.getProject(pid);
      return project.settings ? project.settings[key] : undefined;
    });
  }

  private async setProjectSetting(pid: ProjectID, key: string, value: string): Promise<void> {
    this.setDirty();
    return this.db.transaction("rw", this.db.projects, async () => {
      const project = await this.getProject(pid);
      project.settings = project.settings || {};
      project.settings[key] = value;
      await this.db.projects.update(pid, {
        settings: project.settings,
        last_modified: Date.now()
      });
    });
  }

  private runnerFileKey(question: string): string {
    return `${question}_runner_file`;
  }

  public async getFileToRun(pid: ProjectID, question: string): Promise<string|false> {
    this.debug && console.log(`getFileToRun`);
    return (await this.getProjectSetting(pid, this.runnerFileKey(question))) || false;
  }

  // a file name is (q*/tests|q*|common)/name
  public async setFileToRun(pid: ProjectID, question: string, filename: string): Promise<void> {
    this.debug && console.log(`setFileToRun`);
    return this.setProjectSetting(pid, this.runnerFileKey(question), filename);
  }

  public async getSettings(): Promise<Settings> {
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      this.debug && console.log(`getSettings`);
      const settings = await this.db.settings.get(0);
      return settings ? Settings.fromJSON(settings) : new Settings();
    });
  }

  public async setSettings(settings: Settings): Promise<void> {
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      this.debug && console.log(`setSettings`);
      await this.db.settings.put({
        id: 0,
        editor_mode: settings.editor_mode,
        font_size: settings.font_size,
        font: settings.font,
        theme: settings.theme,
        space_tab: settings.space_tab,
        tab_width: settings.tab_width
      });
    });
  }

  public async getProjectFiles(pid: ProjectID): Promise<FileBrief[]> {
    this.debug && console.log(`getProjectFiles`);
    // this is called when we open a project, so we will update the last modified time here as well
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      const p: Project = await this.getProject(pid);
      p.last_modified = Date.now();
      await this.db.projects.put(p);
      const fbs: FileBrief[] = [];
      await this.db.files.where("project").equals(pid).each((file: File) => {
        fbs.push(new FileBrief(file));
      });
      return fbs;
    });
  }

  public async newFile(pid: ProjectID,
                       name: string,
                       contents = "",
                        // set pushChangeLog = false in applyChanges to make syncAll faster
                       pushChangeLog: boolean = true): Promise<FileBrief> {
    this.debug && console.log(`newFile`);
    const rmatch: RegExpMatchArray | null = contents.match(/^data:([^;]*)?(?:;(?!base64)([^;]*))?(?:;(base64))?,(.*)/);
    if (rmatch !== null) {
      const mime = rmatch[1];
      const b64 = rmatch[3];
      if (b64 || mime === "base64") {
        contents = window.atob(rmatch[4]);
      }
    }
    const checksum = md5(contents);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      /*
        Known problem:
          If you rename a file from A to B, then you won't be able to create a file named A again,
          since rename should not change the original id of the file.
        How to fix:
          Make sure the id is unique for each new file. For example: const id = md5(proj + name + Date.now()).
          This requires the backend to be aware of file ids
      */
      const fid: FileID = this.fileID(pid, name);
      const exist = await this.db.files.where({
        name: name,
        project: pid
      });
      if (await exist.count() > 0) {
        throw new LocalStorageError(`file "${pid}" "${name}" already exists`);
      }
      const fs: FileStored = {
        id: fid,
        project: pid,
        name: name,
        contents: contents,
        checksum: checksum,
        last_modified: Date.now(),
      };
      await this.db.files.add(fs);
      const proj: ProjectBrief = await this.getProject(pid);
      if (pushChangeLog) {
        await this.pushChangeLog({
          type: "newFile",
          contents: contents,
          file: {file: name, project: proj.name},
          checksum: checksum
        });
      }
      return new FileBrief(fs);
    });
  }

  public async newProject(name: string): Promise<ProjectBrief> {
    this.debug && console.log(`newProject`);
    const pid = this.projID(name);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      const ps: ProjectStored = {
        id: pid,
        name: name,
        settings: {},
        last_modified: Date.now(),
      };
      await this.db.projects.add(ps);
      return new ProjectBrief(ps);
    });
  }

  public async deleteProject(pid: ProjectID): Promise<void> {
    this.debug && console.log(`deleteProject`);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      await this.db.projects.delete(pid);
      const files = await this.db.files.where("project").equals(pid);
      await files.delete();
    });
  }

  public async getProject(pid: ProjectID): Promise<Project> {
    this.debug && console.log(`getProject`);
    return await this.db.transaction("r", this.db.projects, async () => {
      const p = await this.db.projects.get(pid);
      if (!p) {
        throw new LocalStorageError(`project "${pid}" doesn't exist`);
      }
      return new Project(p);
    });
  }

  public async getProjects(): Promise<ProjectBrief[]> {
    this.debug && console.log(`getProjects`);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      const projs: ProjectBrief[] = [];
      await this.db.projects.toCollection().each((proj: Project) => {
        projs.push(new ProjectBrief(proj));
      });
      return projs;
    });
  }

  public async getProjectsForSync(): Promise<Project[]> {
    this.debug && console.log(`getProjects`);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      const projs: ProjectBrief[] = [];
      await this.db.projects.toCollection().each((proj: Project) => {
        projs.push(new Project(proj));
      });
      return projs;
    });
  }

  public async getAllFiles(): Promise<FileBrief[]> {
    this.debug && console.log(`getAllFiles`);
    return await this.db.transaction("r", this.db.files, async () => {
      const result = await this.db.files.toArray();
      return R.map((file: File) => new FileBrief(file), result);
    });
  }

  private openFilesKey(question: string): string {
    return `${question}_open_files`;
  }

  private async getFileByName(pid: ProjectID, fname: string): Promise<FileBrief|undefined> {
    let result = await this.db.files.where("name").equals(fname).first();
    return result ? new FileBrief(result) : result;
  }

  public async getOpenFiles(pid: ProjectID, question: string): Promise<FileBrief[]> {
    this.debug && console.log(`getOpenFiles`);
    let openFilesNames = JSON.parse(await this.getProjectSetting(pid, this.openFilesKey(question)) || "[]");
    let openFiles = await
      Promise.all(R.map(async (name: string) => await this.getFileByName(pid, name), openFilesNames));
    return <FileBrief[]>R.filter((x) => !!x, openFiles);
  }

  public async addOpenFile(pid: ProjectID, question: string, fid: FileID): Promise<void> {
    this.debug && console.log(`addOpenFile`);
    return this.db.transaction("rw", [this.db.projects, this.db.files], async () => {
      const open = JSON.parse(await this.getProjectSetting(pid, this.openFilesKey(question)) || "[]");
      const file = new FileBrief(await this.readFile(fid));
      return this.setProjectSetting(
        pid,
        this.openFilesKey(question),
        JSON.stringify(open.concat([file.name])));
    });
  }

  public async removeOpenFile(pid: ProjectID, question: string, fid: FileID): Promise<void> {
    this.debug && console.log(`removeOpenFile`);
    return this.db.transaction("rw", this.db.projects, this.db.files, async () => {
      const openNames = JSON.parse(await this.getProjectSetting(pid, this.openFilesKey(question)) || "[]");
      const removedFile = await this.readFile(fid);
      return this.setProjectSetting(
        pid,
        this.openFilesKey(question),
        JSON.stringify(openNames.filter((f: string) =>
          f !== removedFile.name
        )));
    });
  }

  public async getChangeLogs(): Promise<ChangeLog[]> {
    this.debug && console.log(`getChangeLogs`);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      return await this.db.changeLogs.orderBy("id").reverse().toArray();
    });
  }

  public async topChangeLog(): Promise<ChangeLog|false> {
    this.debug && console.log(`topChangeLog`);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      const log = await this.db.changeLogs.orderBy("id").reverse().limit(1).first();
      return log || false;
    });
  }

  public async pushChangeLog(change: ChangeLog): Promise<number> {
    this.debug && console.log(`pushChangeLog`);
    const top = await this.topChangeLog();
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      if (top && change.type === "editFile" && top.type === "editFile") {
        await this.popChangeLog();
        top.contents = change.contents;
      }
      return await this.db.changeLogs.put(change);
    });
  }

  public async popChangeLog(): Promise<ChangeLog|false> {
    this.debug && console.log(`popChangeLog`);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      const top = await this.topChangeLog();
      if (top) {
        if (top.id) {
          await this.db.changeLogs.delete(top.id);
          return top;
        } else {
          return false;
        }
      } else {
        return false;
      }
    });
  }

  public async countChangeLogs(): Promise<number> {
    this.debug && console.log(`countChangeLogs`);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      return await this.db.changeLogs.count();
    });
  }

  public async clearChangeLogs(): Promise<void> {
    this.debug && console.log(`clearChangeLogs`);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    return await this.db.transaction("rw", tbs, async () => {
      this.db.changeLogs.clear();
    });
  }

  // Will be replaced by Dexie.Syncable.ISyncProtocol
  public async applyChanges(changeLogs: ChangeLog[],
                            newProjects: string[],
                            deletedProjects: ProjectID[]): Promise<void> {
    this.debug && console.log(`applyChanges`);
    const tbs = [this.db.files, this.db.projects, this.db.settings, this.db.changeLogs];
    let result = await this.db.transaction("rw", tbs, async () => {
      Dexie.currentTransaction.on("abort", () => {
        console.warn("applyChanges transaction aborted");
      });
      for (const proj of newProjects) {
        await this.newProject(proj);
      }
      for (const change of changeLogs) {
        const pid = this.projID(change.file.project);
        const fid = this.fileID(pid, change.file.file);
        if (change.type === "deleteFile") {
          await this.deleteFile(fid, false);
        } else if (change.type === "editFile") {
          await this.writeFile(fid, change.contents, change.checksum, false);
        } else if (change.type === "newFile") {
          await this.newFile(pid, change.file.file, change.contents, false);
        } else {
          throw sprintf("applyChanges: unknown change %s!", change);
        }
      }
      for (const pid of deletedProjects) {
        await this.deleteProject(pid);
      };
      await this.clearChangeLogs();
    });
    this.clearDirty();
    return result;
  }
}



interface ChangeLog {
  id?: number;
  type: "newFile" | "deleteFile" | "editFile";
  contents?: string;
  file: {file: string, project: string};
  checksum: string;
}

class StorageDB extends Dexie {
  public changeLogs: Dexie.Table<ChangeLog, number>;
  public files: Dexie.Table<FileStored, FileID>;
  public projects: Dexie.Table<ProjectStored, ProjectID>;
  public settings: Dexie.Table<SettingsStored, number>;

  public constructor(dbName: string, options?: DBOptions) {
    super(dbName, options);
    this.version(2).stores({
      changeLogs: "$$id",
      files: "$$id, [name+project], [project+open], name, project",
      projects: "$$id, name",
      settings: "$$id"
    });

    // No TS bindings for Dexie.Syncable
    (<any>this).syncable.connect("seashell", null);
    (<any>this).syncable.on("statusChanged", (newStatus: any, url: string) => {
      console.log(`Sync status changed: ${(<any>Dexie).Syncable.StatusTexts[newStatus]}`);
    });
  }
}
