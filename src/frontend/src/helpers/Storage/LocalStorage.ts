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

/*
  In the storage interface, a filename is always a path from the root of the project.
  These can have three basic forms:

  {question}/{file}
  {question}/tests/{file}
  common/{file}
*/

class LocalStorage {

  private db: StorageDB;
  private dbName: string;
  private isConnected: Function;

  public constructor(public debug = false, isConnected: Function) {
    this.isConnected = isConnected;
  }

  /* connect(dbName)
    Connects to the IndexedDB database.

    Args:
    dbName - Name of database to connect to */
  public async connect(dbName: string): Promise<void> {
    this.dbName = dbName;
    this.db = new StorageDB(dbName, {
      IDBKeyRange: (<any>window).IDBKeyRange,
      indexedDB: (<any>window).indexedDB
    }, this.isConnected);

    await this.db.open();
  }

  /* deleteDB()
    Deletes the entire IndexedDB database. */
  public async deleteDB(): Promise<void> {
    // return this.db.delete();
  }

  /* writeFile(fid, contents)
    Overwrites the contents of an existing file.

    Args:
     fid - The file ID of the file we are overwriting.
     contents - the new contents of the file
    Returns:
     The new file ID associated with the file */
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

  /* readFile(fid, contents)
    Reads a file entry form the database.

    Args:
     fid - The file ID to read from.
     contents - true to grab the associated contents, false to leave contents undefined
    Returns:
     The FileEntry object associated with the file */
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

  /* getFiles(pid, question, contents)
    Fetches a list of all files in a certain project or question.

    Args:
     pid - project ID
     question - (Optional) Name of question to get files for. If undefined, gets all files for the project.
     contents - (Optional) Set to true to fetch file contents as well. Default false.
    Returns:
     An array of the corresponding File objects */
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

  /* getQuestions(pid)
    Fetches the list of questions for a given project.

    Args:
     pid - project ID
    Returns:
     An array of strings representing the question names */
  public async getQuestions(pid: ProjectID): Promise<string[]> {
    this.debug && console.log("getQuestions");
    return this.db.transaction("r", this.db.files, async () => {
      let res = await this.db.files.where("project_id").equals(pid).toArray();
      return res.filter((item: FileStored) =>
          !item.contents_id && item.name !== "common" && item.name.indexOf("/") === -1)
        .map((item: FileStored) => item.name).sort();
    });
  }

  /* getFileByName(pid, filename, contents)
    Reads a file from the database by filename, if it exists.

    Args:
     pid - project ID
     filename - the filename within the project we are attempting to read, such as "q1/main.c"
     contents - (Optional) Set false if you do not need the file contents. Default true.
    Returns:
     If a file with that name exists, returns the corresponding FileEntry.
     If a file with that name does not exist, throws a StorageError.
     If the contents for that file does not exist, throws a StorageError.
     If there are multiple rows in the files table matching that filename, throws a ConflictError. */
  public async getFileByName(pid: ProjectID, filename: string, contents: boolean = true): Promise<FileEntry> {
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
        throw new E.StorageError(`File ${filename} does not exist.`);
      } else { // file exists, and no conflict
        let file = new FileEntry(result[0]);
        if (contents && file.contents_id) {
          let cnts = await this.db.contents.get(file.contents_id);
          if (cnts === undefined) {
            throw new E.StorageError(`File ${filename} has no contents!`);
          } else {
            file.contents = new Contents(file.contents_id, cnts);
          }
        }
        return file;
      }
    });
  }

  /* resolveConflict(contents)
    Overwrites any existing contents with the same filename with the given contents.
    Uses the Contents object's filename.

    Args:
     contents - The Contents object that will become the current contents for the file */
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

  /* deleteFile(pid, filename)
    Deletes a file.

    Args:
     pid - project ID
     filename - name of file to delete, such as "q1/main.c" */
  public async deleteFile(pid: ProjectID, filename: string): Promise<void> {
    this.debug && console.log("deleteFile");
    return await this.db.transaction("rw", this.db.files, this.db.contents, async () => {
      const file = await this.getFileByName(pid, filename, false);
      if (!file) {
        throw new E.StorageError(`Deleting file ${filename} which does not exist.`);
      } else {
        return this.db.files.delete(file.id);
      }
    });
  }

  /* renameFile(pid, currentName, newName)
    Renames an existing file. Implemented by deleting and creating a new file.

    Args:
     pid - project ID
     currentName - name of the file to be renamed, such as "q1/main.c"
     newName - new name of the file
    Returns:
      A FileEntry object representing the renamed file */
  public async renameFile(pid: ProjectID, currentName: string, newName: string): Promise<FileEntry> {
    this.debug && console.log("renameFile");
    return await this.db.transaction("rw", this.db.contents, this.db.files, async () => {
      let file = await this.getFileByName(pid, currentName);
      if (!file) {
        throw new E.StorageError(`Renaming file ${currentName} which does not exist.`);
      }
      await this.deleteFile(pid, file.name);
      let nFile = await this.newFile(pid, newName, file.contents ? file.contents.contents : "");
      return new FileEntry(nFile);
    });
  }

  /* getVersions(pid, filename)
    Fetches a list of all previous revisions of a file in the database.

    Args:
     pid - project ID
     filename - name of file to get versions for, such as "q1/main.c"
    Returns:
     An array of Contents objects, each represents a previous version of the file */
  public async getVersions(pid: ProjectID, filename: string): Promise<Contents[]> {
    this.debug && console.log("getVersions");
    return this.db.transaction("r", this.db.contents, async () => {
      let result = await this.db.contents.where("[project_id+filename]")
        .equals([pid, filename]).toArray();
      return result.map((vrs: ContentsStored) => new Contents(vrs.id as ContentsID, vrs))
        .sort((a: Contents, b: Contents) => b.time - a.time);
    });
  }

  /* getProjectSetting(pid, key)
    Gets the value of a specific project setting.

    Args:
     pid - project ID
     key - The setting key you are looking up, such as "q1_runner_file"
    Returns:
     The value associated with the key, if it exists.
     If it does not exist, returns undefined. */
  private async getProjectSetting(pid: ProjectID, key: string): Promise<string|undefined> {
    return this.db.transaction("r", this.db.projects, async () => {
      const project = await this.getProject(pid);
      return project.settings ? project.settings[key] : undefined;
    });
  }

  /* setProjectSetting(pid, key, value)
   Sets the value of a specific project setting

   Args:
    pid - project ID
    key - The setting key you want to set
    value - The new value of the setting */
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

  /* runnerFileKey(question)
   Returns the project setting key which stores the runner file for the given question. */
  private runnerFileKey(question: string): string {
    return `${question}_runner_file`;
  }

  /* getFileToRun(pid, question)
   Returns the runner file's filename for the given question. */
  public async getFileToRun(pid: ProjectID, question: string): Promise<string|false> {
    this.debug && console.log("getFileToRun");
    return (await this.getProjectSetting(pid, this.runnerFileKey(question))) || false;
  }

  /* setFileToRun(pid, question, filename)
   Sets the runner file for the given question.

   Args:
    pid - project ID
    question - name of question
    filename - name of new runner file, such as "q1/main.c" */
  public async setFileToRun(pid: ProjectID, question: string, filename: string): Promise<void> {
    this.debug && console.log("setFileToRun");
    return this.setProjectSetting(pid, this.runnerFileKey(question), filename);
  }

  /* getSettings()
   Returns the global settings object for the current user. */
  public async getSettings(): Promise<Settings> {
    this.debug && console.log("getSettings");
    return await this.db.transaction("r", this.db.settings, async () => {
      this.debug && console.log(`getSettings`);
      const settings = await this.db.settings.get("settings-key");
      return settings ? Settings.fromJSON(settings) : new Settings();
    });
  }

  /* setSettings(settings)
   Sets the global settings object for the current user. */
  public async setSettings(settings: Settings): Promise<void> {
    this.debug && console.log("setSettings");
    return await this.db.transaction("rw", this.db.settings, async () => {
      this.debug && console.log(`setSettings`);
      await this.db.settings.put({
        id: "settings-key",
        editor_mode: settings.editor_mode,
        font_size: settings.font_size,
        font: settings.font,
        theme: settings.theme,
        space_tab: settings.space_tab,
        tab_width: settings.tab_width
      });
    });
  }

  /* newFile(pid, name, contents)
   Creates a new file.

   Args:
    pid - project ID
    name - name of the file to create
    contents - (Optional) The initial contents of the created file. Defaults to empty string.
   Returns:
    A FileEntry object for the new file */
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

  /* newQuestion(pid, question)
   Creates a new question.

   Args:
    pid - project ID
    question - name of the question to create */
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

  /* deleteQuestion(pid, question)
    Deletes a question.

    Args:
     pid - project ID
     question - name of the question to delete */
  public async deleteQuestion(pid: ProjectID, question: string): Promise<void> {
    this.debug && console.log("deleteQuestion");
    // delete the directory file entry and all children files
    await this.db.transaction("rw", this.db.files, () => {
      return this.db.files.where("name").startsWith(question).delete();
    });
  }

  /* newProject(name)
    Creates a new project.

    Args:
     name - name of the project to create
    Returns:
     A Project object for the new project */
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

  /* deleteProject(pid)
    Deletes a project.

    Args:
     pid - project ID to delete */
  public async deleteProject(pid: ProjectID): Promise<void> {
    this.debug && console.log("deleteProject");
    return await this.db.transaction("rw", [this.db.files, this.db.projects], async () => {
      await this.db.projects.delete(pid);
      const files = await this.db.files.where("project_id").equals(pid);
      await files.delete();
    });
  }

  /* getProject(pid)
    Fetches a Project object from the database.

    Args:
     pid - project ID to fetch
    Returns:
     Project object */
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

  /* getProjects()
    Gets a list of all projects.

    Returns:
     An array of Project objects */
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

  /* updateLastUsed(pid)
    Sets the last used time for a project to the current time.

    Args:
     pid - project ID to update */
  public updateLastUsed(pid: ProjectID): Promise<number> {
    this.debug && console.log("updateLastUsed");
    return this.db.transaction("rw", this.db.projects, () => {
      return this.db.projects.update(pid, {last_used: Date.now()});
    });
  }

  /* getAllFiles()
    Gets a list of all files for all projects.

    Args:
     contents - (Optional) Set true to also fetch contents of all files. Default false.
    Returns:
     An array of File objects */
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

  /* openFilesKey(question)
    Returns the project setting key for the list of open files in the given question. */
  private openFilesKey(question: string): string {
    return `${question}_open_files`;
  }

  /* getOpenFiles(pid, question)
    Gets the currently open files in a question.

    Args:
     pid - project ID
     question - question name
    Returns:
     Array of open filenames */
  public async getOpenFiles(pid: ProjectID, question: string): Promise<string[]> {
    this.debug && console.log("getOpenFiles");
    return this.db.transaction("r", this.db.projects, async () => {
      return JSON.parse(await this.getProjectSetting(pid, this.openFilesKey(question)) || "[]").sort();
    });
  }

  /* addOpenFile(pid, question, filename)
    Adds a file to the list of open files for a question.

    Args:
     pid - project ID
     question - question name
     filename - filename to add to the list */
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

  /* removeOpenFile(pid, question, filename)
    Removes a file from the list of open files for a question (ie. closes the file)

    Args:
     pid - project ID
     question - question name
     filename - name of file to remove from the list */
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

  /* waitForSync()
    Triggers a sync with the backend server if we are connected.
    Promise resolves when the sync is complete. */
  public waitForSync(): Promise<void> {
    return this.db.waitForSync();
  }

  /* exportAsZip(pid, question)
    Exports Seashell files as a .zip file.

    Args:
     pid - (Optional) project ID to export. If undefined, will export all projects.
     question - (Optional) question name to export. If undefined, will export all questions for the given project.
    Returns:
     JSZip object representing the result */
  public async exportAsZip(pid: ProjectID | undefined = undefined, question: string | undefined = undefined): Promise<JSZip> {
    return this.db.transaction("r", [this.db.projects, this.db.files, this.db.contents], async () => {
      let result = new JSZip();
      let files = await (pid ? this.getFiles(pid, question, true) : this.getAllFiles(true));
      // export the files themselves
      await Promise.all(files.map(async (file: File) => {
        result.file(pid ? file.name : ((await this.getProject(file.project_id)).name) + "/" + file.name,
                    file.contents === false ? "" : file.contents.contents);
      }));
      // create the metadata file if we are exporting a single project
      if (pid) {
        let psettings: any = (await this.getProject(pid)).settings;
        psettings.flags = files.map((file: File) => [file.name, file.flags ? file.flags : 0]);
        result.file(".metadata", JSON.stringify(psettings));
      }
      return result;
    });
  }
}

/* Class defining database schema and initializing the sync protocol */
class StorageDB extends Dexie {
  public contents: Dexie.Table<ContentsStored, ContentsID>;
  public files: Dexie.Table<FileStored, FileID>;
  public projects: Dexie.Table<ProjectStored, ProjectID>;
  public settings: Dexie.Table<SettingsStored, string>;

  private isConnected: Function;

  /* StorageDB(dbName, options, isConnected)
    Args:
     dbName - name of the IndexedDB database
     options - Dexie options
     isConnected - predicate function that determines if the websocket is connected */
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

  /* waitForSync()
    Triggers a sync by disconnecting & reconnecting, then teturns a promise
    that resolves when the database changes to ONLINE. */
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
