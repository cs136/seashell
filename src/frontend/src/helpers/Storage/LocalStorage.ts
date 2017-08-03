import Dexie from "dexie";
import "dexie-observable";
import "dexie-syncable";
import md5 = require("md5");
import * as R from "ramda";
import {Contents, ContentsID, ContentsStored,
        File, FileID, FileStored, FileEntry,
        Project, ProjectID, ProjectStored,
        Settings, SettingsStored} from "./Interface";
import * as E from "../Errors";
import JSZip = require("jszip");

export {LocalStorage}


interface DBOptions {
  addons?: Array<(db: Dexie) => void>;
  autoOpen?: boolean;
  indexedDB?: IDBFactory;
  IDBKeyRange?: new () => IDBKeyRange;
}

class LocalStorage {

  private db: StorageDB;
  private dbName: string;
  private isConnected: Function;

  public constructor(public debug = false, isConnected: Function) {
    this.isConnected = isConnected;
  }

  public async connect(dbName: string): Promise<void> {
    this.dbName = dbName;
    this.db = new StorageDB(dbName, {
      IDBKeyRange: (<any>window).IDBKeyRange,
      indexedDB: (<any>window).indexedDB
    }, this.isConnected);

    await this.db.open();
  }

  public async deleteDB(): Promise<void> {
    return this.db.delete();
  }

  public async writeFile(fid: FileID, contents: string): Promise<FileID> {
    this.debug && console.log("writeFile");
    return this.db.transaction("rw", [this.db.contents, this.db.files], async () => {
      let file: File = await this.readFile(fid, false);
      if (!file) {
        throw new E.StorageError(`File ID ${fid} does not exist.`);
      }
      await this.deleteFile(file.project_id, file.name);
      let nFile = await this.newFile(file.project_id, file.name, contents);
      return nFile.id;
    });
  }

  public async readFile(fid: FileID, contents: boolean = true): Promise<FileEntry> {
    this.debug && console.log("readFile");
    let tbls: Dexie.Table<any, string>[] = [this.db.files];
    if (contents) tbls.push(this.db.contents);
    return await this.db.transaction("r", tbls, async () => {
      const file = await this.db.files.get(fid);
      if (!file) {
        throw new E.StorageError(`File "${fid}" does not exist.`);
      }
      let result = new FileEntry(file);
      if (contents && result.contents_id) {
        const conts = await this.db.contents.get(result.contents_id);
        if (!conts) {
          throw new E.StorageError(`file ${file.name} has invalid contents.`);
        }
        result.contents = new Contents(result.contents_id, conts);
      }
      return result;
    });
  }

  public async getFiles(pid: ProjectID, question: string | undefined = undefined, contents: boolean = false): Promise<File[]> {
    this.debug && console.log("getFiles");
    const tables = contents ?
      [this.db.files, this.db.contents] :
      [this.db.files];
    return this.db.transaction("r", tables, async () => {
      let res = (await this.db.files.where("project_id").equals(pid).toArray())
        .filter((item: FileStored) => item.contents_id);
      if (question) {
        res = res.filter((item: FileStored) =>
          item.name.startsWith(question) || item.name.startsWith("common"));
      }
      let result: File[] = [];
      if (contents) {
        result = await Promise.all(res.map((item: FileStored) => this.readFile(item.id as FileID)));
      }
      else {
        result = res.map((item: FileStored) => new File(item));
      }
      return result.sort((a: File, b: File) => {
        if (a.name < b.name) return -1;
        if (a.name > b.name) return 1;
        return 0;
      });
    });
  }

  public async getQuestions(pid: ProjectID): Promise<string[]> {
    this.debug && console.log("getQuestions");
    return this.db.transaction("r", this.db.files, async () => {
      let res = await this.db.files.where("project_id").equals(pid).toArray();
      return res.filter((item: FileStored) =>
          !item.contents_id && item.name !== "common" && item.name.indexOf("/") === -1)
        .map((item: FileStored) => item.name).sort();
    });
  }

  public async getFileByName(pid: ProjectID, filename: string, contents: boolean = true): Promise<FileEntry|false> {
    this.debug && console.log("getFileByName");
    return this.db.transaction("r", this.db.files, this.db.contents, async () => {
      let result = await this.db.files.where("[name+project_id]")
        .equals([filename, pid]).toArray();
      // Now is the time to detect any file conflict on this filename
      if (result.length > 1) {
        let conflictContents = await Promise.all(result.map(async (file) => {
          if (file.contents_id) {
            let conts = await this.db.contents.get(file.contents_id);
            if (conts) {
              return new Contents(file.contents_id, conts);
            } else {
              throw new E.StorageError("Error when detecting file conflict.", result);
            }
          } else {
            throw new E.StorageError("Error when detecting file conflict.", result);
          }
        }));
        throw new E.ConflictError(filename, conflictContents);
      } else if (result.length === 0) { // file does not exist
        return false;
      } else { // file exists, and no conflict
        let file = new FileEntry(result[0]);
        if (contents && file.contents_id) {
          let cnts = await this.db.contents.get(file.contents_id);
          if (cnts === undefined) {
            throw new E.StorageError(`File contents for ${filename} does not exist.`);
          } else {
            file.contents = new Contents(file.contents_id, cnts);
          }
        }
        return file;
      }
    });
  }

  public async resolveConflict(contents: Contents): Promise<void> {
    this.debug && console.log("resolveConflict");
    return this.db.transaction("rw", this.db.files, async () => {
      let res = await this.db.files.where("[name+project_id]")
        .equals([contents.filename, contents.project_id]).toArray();
      if (res.length < 1) {
        throw new E.StorageError("Resolving conflict on file that does not exist.");
      }
      await this.db.files.where("[name+project_id]")
        .equals([contents.filename, contents.project_id]).delete();
      await this.db.files.add({
        project_id: contents.project_id,
        name: contents.filename,
        contents_id: contents.id,
        flags: res[0].flags
      });
    });
  }

  public async deleteFile(project: ProjectID, filename: string): Promise<void> {
    this.debug && console.log("deleteFile");
    return await this.db.transaction("rw", this.db.files, this.db.contents, async () => {
      const file = await this.getFileByName(project, filename, false);
      if (!file) {
        throw new E.StorageError(`Deleting file ${filename} which does not exist.`);
      } else {
        return this.db.files.delete(file.id);
      }
    });
  }

  public async renameFile(project: ProjectID, currentName: string, newName: string): Promise<FileEntry> {
    this.debug && console.log("renameFile");
    return await this.db.transaction("rw", this.db.contents, this.db.files, async () => {
      let file = await this.getFileByName(project, currentName);
      if (!file) {
        throw new E.StorageError(`Renaming file ${currentName} which does not exist.`);
      }
      await this.deleteFile(project, file.name);
      let nFile = await this.newFile(project, newName, file.contents ? file.contents.contents : "");
      return new FileEntry(nFile);
    });
  }

  public async getVersions(pid: ProjectID, filename: string): Promise<Contents[]> {
    this.debug && console.log("getVersions");
    return this.db.transaction("r", this.db.contents, async () => {
      let result = await this.db.contents.where("[project_id+filename]")
        .equals([pid, filename]).toArray();
      return result.map((vrs: ContentsStored) => new Contents(vrs.id as ContentsID, vrs))
        .sort((a: Contents, b: Contents) => b.time - a.time);
    });
  }

  private async getProjectSetting(pid: ProjectID, key: string): Promise<string|undefined> {
    return this.db.transaction("r", this.db.projects, async () => {
      const project = await this.getProject(pid);
      return project.settings ? project.settings[key] : undefined;
    });
  }

  private async setProjectSetting(pid: ProjectID, key: string, value: string): Promise<void> {
    return this.db.transaction("rw", this.db.projects, async () => {
      const project = await this.getProject(pid);
      project.settings = project.settings || {};
      project.settings[key] = value;
      await this.db.projects.update(pid, {
        settings: project.settings,
        last_used: Date.now()
      });
    });
  }

  private runnerFileKey(question: string): string {
    return `${question}_runner_file`;
  }

  public async getFileToRun(pid: ProjectID, question: string): Promise<string|false> {
    this.debug && console.log("getFileToRun");
    return (await this.getProjectSetting(pid, this.runnerFileKey(question))) || false;
  }

  // a file name is (q*/tests|q*|common)/name
  public async setFileToRun(pid: ProjectID, question: string, filename: string): Promise<void> {
    this.debug && console.log("setFileToRun");
    return this.setProjectSetting(pid, this.runnerFileKey(question), filename);
  }

  public async getSettings(): Promise<Settings> {
    this.debug && console.log("getSettings");
    return await this.db.transaction("r", this.db.settings, async () => {
      this.debug && console.log(`getSettings`);
      const settings = await this.db.settings.get(0);
      return settings ? Settings.fromJSON(settings) : new Settings();
    });
  }

  public async setSettings(settings: Settings): Promise<void> {
    this.debug && console.log("setSettings");
    return await this.db.transaction("rw", this.db.settings, async () => {
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

  public async newFile(pid: ProjectID, name: string, contents: string = ""): Promise<FileEntry> {
    this.debug && console.log("newFile");
    const rmatch: RegExpMatchArray | null = contents.match(/^data:([^;]*)?(?:;(?!base64)([^;]*))?(?:;(base64))?,(.*)/);
    if (rmatch !== null) {
      const mime = rmatch[1];
      const b64 = rmatch[3];
      if (b64 || mime === "base64") {
        contents = window.atob(rmatch[4]);
      }
    }
    return await this.db.transaction("rw", [this.db.contents, this.db.files], async () => {
      const exist = await this.db.files.where({
        name: name,
        project_id: pid
      });
      if (await exist.count() > 0) {
        throw new E.StorageError(`File "${name}" already exists.`);
      }
      const fid = await this.db.files.add({
        project_id: pid,
        name: name,
        contents_id: false,
        flags: 0
      });
      const cid = await this.db.contents.add({
        project_id: pid,
        filename: name,
        contents: contents,
        time: Date.now()
      });
      await this.db.files.update(fid, {
        contents_id: cid
      });
      return this.readFile(fid, false);
    });
  }

  public async newQuestion(pid: ProjectID, question: string): Promise<void> {
    this.debug && console.log("newQuestion");
    await this.db.transaction("rw", this.db.files, () => {
      return this.db.files.add({
        project_id: pid,
        name: question,
        contents_id: false,
        flags: 0
      });
    });
  }

  public async deleteQuestion(pid: ProjectID, question: string): Promise<void> {
    this.debug && console.log("deleteQuestion");
    // delete the directory file entry and all children files
    await this.db.transaction("rw", this.db.files, () => {
      return this.db.files.where("name").startsWith(question).delete();
    });
  }

  public async newProject(name: string): Promise<Project> {
    this.debug && console.log("newProject");
    return await this.db.transaction("rw", this.db.projects, async () => {
      const num = await this.db.projects.where("name").equals(name).count();
      if (num > 0) {
        throw new E.StorageError(`Project '${name}' already exists.`);
      }
      const ps: ProjectStored = {
        name: name,
        settings: {},
        last_used: Date.now()
      };
      const pid = await this.db.projects.add(ps);
      return new Project(ps);
    });
  }

  public async deleteProject(pid: ProjectID): Promise<void> {
    this.debug && console.log("deleteProject");
    return await this.db.transaction("rw", [this.db.files, this.db.projects], async () => {
      await this.db.projects.delete(pid);
      const files = await this.db.files.where("project_id").equals(pid);
      await files.delete();
    });
  }

  public async getProject(pid: ProjectID): Promise<Project> {
    this.debug && console.log("getProject");
    return await this.db.transaction("r", this.db.projects, async () => {
      const p = await this.db.projects.get(pid);
      if (!p) {
        throw new E.StorageError(`project "${pid}" doesn't exist`);
      }
      return new Project(p);
    });
  }

  public async getProjects(): Promise<Project[]> {
    this.debug && console.log("getProjects");
    return await this.db.transaction("r", this.db.projects, async () => {
      const projs: Project[] = [];
      await this.db.projects.toCollection().each((proj: Project) => {
        projs.push(new Project(proj));
      });
      return projs.sort((a: Project, b: Project) => b.last_used - a.last_used);
    });
  }

  public updateLastUsed(pid: ProjectID): Promise<void> {
    this.debug && console.log("updateLastUsed");
    return this.db.transaction("rw", this.db.projects, () => {
      return this.db.projects.update(pid, {last_used: Date.now()});
    });
  }

  public async getAllFiles(contents: boolean = false): Promise<File[]> {
    this.debug && console.log("getAllFiles");
    let tables = contents ? [this.db.files, this.db.contents] : [this.db.files];
    return await this.db.transaction("r", tables, async () => {
      let files = (await this.db.files.toArray()).filter((file: FileStored) => file.contents_id);
      if (contents) {
        return Promise.all(files.map((file: FileStored) => this.readFile(file.id as FileID)));
      } else {
        return files.map((file: FileStored) => new File(file));
      }
    });
  }

  private openFilesKey(question: string): string {
    return `${question}_open_files`;
  }

  public async getOpenFiles(pid: ProjectID, question: string): Promise<string[]> {
    this.debug && console.log("getOpenFiles");
    return this.db.transaction("r", this.db.projects, async () => {
      return JSON.parse(await this.getProjectSetting(pid, this.openFilesKey(question)) || "[]").sort();
    });
  }

  public async addOpenFile(pid: ProjectID, question: string, filename: string): Promise<void> {
    this.debug && console.log("addOpenFile");
    return this.db.transaction("rw", this.db.projects, async () => {
      const open = JSON.parse(await this.getProjectSetting(pid, this.openFilesKey(question)) || "[]");
      return this.setProjectSetting(
        pid,
        this.openFilesKey(question),
        JSON.stringify(open.concat([filename])));
    });
  }

  public async removeOpenFile(pid: ProjectID, question: string, filename: string): Promise<void> {
    this.debug && console.log("removeOpenFile");
    return this.db.transaction("rw", this.db.projects, async () => {
      const openNames = JSON.parse(await this.getProjectSetting(pid, this.openFilesKey(question)) || "[]");
      return this.setProjectSetting(
        pid,
        this.openFilesKey(question),
        JSON.stringify(openNames.filter((f: string) =>
          f !== filename
        )));
    });
  }

  public waitForSync(): Promise<void> {
    return this.db.waitForSync();
  }

  public async exportAsZip(pid: ProjectID | undefined = undefined, question: string | undefined = undefined): Promise<JSZip> {
    return this.db.transaction("r", [this.db.projects, this.db.files, this.db.contents], async () => {
      let files = await (pid ? this.getFiles(pid, question, true) : this.getAllFiles(true));
      let result = new JSZip();
      await Promise.all(files.map(async (file: File) => {
        result.file(pid ? file.name : ((await this.getProject(file.project_id)).name) + "/" + file.name,
                    file.contents === false ? "" : file.contents.contents);
      }));
      return result;
    });
  }
}

class StorageDB extends Dexie {
  public contents: Dexie.Table<ContentsStored, ContentsID>;
  public files: Dexie.Table<FileStored, FileID>;
  public projects: Dexie.Table<ProjectStored, ProjectID>;
  public settings: Dexie.Table<SettingsStored, number>;

  private isConnected: Function;

  public constructor(dbName: string, options: DBOptions, isConnected: Function) {
    super(dbName, options);
    this.version(1).stores({
      contents: "$$id, project_id, filename, [project_id+filename]",
      files: "$$id, [name+project_id], name, project_id",
      projects: "$$id, name",
      settings: "$$id"
    });

    this.isConnected = isConnected;
    this.syncable.connect("seashell", "http://no-host.org");
  }

  // Returns a promise that resolves when the database changes to ONLINE.
  public async waitForSync() {
    if (this.isConnected()) {
      try {
        await this.syncable.disconnect("http://no-host.org");
        await this.syncable.connect("seashell", "http://no-host.org");
      } catch (e) {
        console.warn("Using stale contents -- waitForSync failed with %s.", e);
        throw new E.StorageError("Error syncing with the backend -- " + e);
      }
    }
  }
}
