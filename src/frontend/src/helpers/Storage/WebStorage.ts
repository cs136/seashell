import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import {LocalStorage, ChangeLog} from "./LocalStorage";
import {AbstractStorage, AbstractWebStorage,
        Project, ProjectID, ProjectBrief,
        File, FileID, FileBrief,
        Settings, defaultSettings} from "./Interface";
import {History, Change} from "../types";
import * as E from "../Errors";
export {WebStorage, SeashellWebsocket}
import md5 = require("md5");
import * as R from "ramda";

enum OfflineMode {Off, On, Forced}

class Callback {
  constructor(public type: string, public cb: (message?: any) => any, public now: boolean) { }
}



class WebStorage extends AbstractStorage implements AbstractWebStorage {
  private socket: SeashellWebsocket;
  private storage: LocalStorage;

  // public newProjectFrom: (name: string, src_url: string) => Promise<WebsocketResult>;
  // public deleteProject: (name: string) => Promise<WebsocketResult>;
  // public restoreFileFrom: (name: string, file: string, contents: string, history: History) => Promise<WebsocketResult>;
  // public getUploadFileToken: (name: string, file: string) => Promise<WebsocketResult>;
  // public getExportToken: (name: string) => Promise<WebsocketResult>;
  // public marmosetSubmit: (name: string, assn: string, subdir?: string) => Promise<WebsocketResult>;

  // public lockProject: (name: string) => Promise<WebsocketResult>;
  // public forceLockProject: (name: string) => Promise<WebsocketResult>;
  // public unlockProject: (name: string) => Promise<WebsocketResult>;
  // public archiveProjects: () => Promise<WebsocketResult>;

  // public getSettings: () => Promise<WebsocketResult>;
  // public getMostRecentlyUsed: (proj: string, question: string) => Promise<WebsocketResult>;
  // public updateMostRecentlyUsed: (proj: string, question: string, file: string) => Promise<WebsocketResult>;

  public debug: boolean; // toggle this.debug && console.log

  constructor(wbclient: SeashellWebsocket, store: LocalStorage, debug?: boolean) {
    super();
    // this.synced = false;
    // this.isSyncing = false;
    // this.offlineMode = 0; // TODO change this to look up the cookie
    this.storage = store;
    this.socket = wbclient;
    this.debug = debug;

    // These functions are not available in offline mode.
    // this.ping = this.make_offline_disabled("ping");
    // this.newProject = this.make_offline_disabled("newProject");/
    // this.newProjectFrom = this.make_offline_disabled("newProjectFrom");
    // this.deleteProject = this.make_offline_disabled("deleteProject");
    // this.restoreFileFrom = this.make_offline_disabled("restoreFileFrom");
    // this.getUploadFileToken = this.make_offline_disabled("getUploadFileToken");
    // this.getExportToken = this.make_offline_disabled("getExportToken");
    // this.marmosetSubmit = this.make_offline_disabled("marmosetSubmit");
    // These functions do nothing and just resolve in offline mode.
    // this.lockProject = this.make_offline_noop("lockProject");
    // this.forceLockProject = this.make_offline_noop("forceLockProject");
    // this.unlockProject = this.make_offline_noop("unlockProject");
    // this.archiveProjects = this.make_offline_noop("archiveProjects");

    // These functions either:
    //  - return the online result if online.
    //  - return the offline result if offline.
    // this.getProjects = this.make_offline_enabled("getProjects");
    // this.getFiles = this.make_offline_enabled("getFiles");
    // this.readFile = this.make_offline_enabled("readFile");
    // this.getFileToRun = this.make_offline_enabled("getFileToRun");
    // this.getSettings = this.make_offline_enabled("getSettings");
    // this.getMostRecentlyUsed = this.make_offline_enabled("getMostRecentlyUsed");

    // These functions:
    //  - invoke the offline version if offline
    //  - invoke the both the offline version and the online
    //    version if online, returning the offline version.
    //  - marks the local storage as dirty and queues up a sync
    // this.newFile = this.make_offline_enabled("newFile", true);
    // this.writeFile = this.make_offline_enabled("writeFile", true);
    // this.deleteFile = this.make_offline_enabled("deleteFile", true);
    // this.renameFile = this.make_offline_enabled("renameFile", true);
    // this.setSettings = this.make_offline_enabled("setSettings", true);
    // this.updateMostRecentlyUsed = this.make_offline_enabled("updateMostRecentlyUsed", true)
  }

  public async newFile(proj: ProjectID, filename: string, contents?: string): Promise<FileID> {
    const fid = await this.storage.newFile(proj, filename, contents);
    await this.socket.sendMessage({
      type: "newFile",
      project: proj,
      file: filename,
      contents: contents || "",
      encoding: "raw",
      normalize: false
    });
    return fid;
  };

  public async renameFile(fid: FileID, newName: string): Promise<void> {
    const file = await this.storage.readFile(fid);
    await this.storage.renameFile(fid, newName);
    await this.socket.sendMessage({
      type: "renameFile",
      project: file.project,
      oldName: file.name,
      newName: newName
    });
  };

  public async deleteFile(fid: FileID): Promise<void> {
    const file = await this.storage.readFile(fid);
    await this.storage.deleteFile(fid);
    await this.socket.sendMessage({
      type: "deleteFile",
      project: file.project,
      file: file.name
    });
  };

  public async deleteProject(pid: ProjectID): Promise<void> {
    const proj = await this.storage.getProject(pid);
    await this.storage.deleteProject(pid);
    await this.socket.sendMessage({
      type: "deleteProject",
      project: proj.name
    });
  };

  public async newProject(name: string): Promise<ProjectID> {
    await this.storage.newProject(name);
    await this.socket.sendMessage({
      type: "newProject",
      project: name
    });
    return name;
  }

  public async getProjects(): Promise<ProjectBrief[]> {
    await this.syncAll();
    return this.storage.getProjects();
  }

  public async getProject(pid: ProjectID): Promise<Project> {
    return this.storage.getProject(pid);
  }

  public async readFile(fid: FileID): Promise<File> {
    return this.storage.readFile(fid);
  }

  public async writeFile(fid: FileID, contents: string): Promise<void> {
    const file = await this.storage.readFile(fid);
    await this.storage.writeFile(fid, contents);
    await this.socket.sendMessage({
      type: "writeFile",
      file: file.name,
      project: file.project,
      contents: contents,
      history: ""
    });
  }

  public async getProjectFiles(pid: ProjectID): Promise<FileBrief[]> {
    await this.syncAll();
    return this.storage.getProjectFiles(pid);
  };

  public async getAllFiles(): Promise<FileBrief[]> {
    await this.syncAll();
    return this.storage.getAllFiles();
  };

  public async getFileToRun(proj: ProjectID, question: string): Promise<FileID|false> {
    return this.storage.getFileToRun(proj, question);
  };

  public async setFileToRun(proj: ProjectID, question: string, id: FileID): Promise<void> {
    return this.storage.setFileToRun(proj, question, id);
  };

  public async getOpenTabs(proj: ProjectID, question: string): Promise<FileID[]> {
    return this.storage.getOpenTabs(proj, question);
  }

  public async setOpenTabs(proj: ProjectID, question: string, files: FileID[]): Promise<void> {
    return this.storage.setOpenTabs(proj, question, files);
  }

  public async setSettings(settings: Settings): Promise<void> {
    return this.storage.setSettings(settings);
  }

  public async getSettings(): Promise<Settings> {
    return this.storage.getSettings();
  }

  // to be replaced by dexie
  public async syncAll(): Promise<void> {
    // this.isSyncing = true;
    const startTime = Date.now();
    const projectsSent = await this.storage.getProjects();
    const filesSent = await this.storage.getAllFiles();
    const changesSent = await this.storage.getChangeLogs();
    const settingsSent = await this.storage.getSettings();
    const result = await this.socket.sendMessage<{
      changes: ChangeLog[],
      newProjects: string[],
      deletedProjects: string[]
    }>({
      type: "sync",
      projects: projectsSent,
      files: filesSent.map((file) => ({
        file: file.name,
        project: file.project,
        checksum: file.checksum
      })),
      changes: changesSent,
      settings: {
        modified: Date.now(),
        values: JSON.stringify(settingsSent),
        name: "settings"
      }
    });
    const changesGot = result.changes;
    const newProjects = result.newProjects;
    const deletedProjects = result.deletedProjects;
    await this.storage.applyChanges(changesGot, newProjects, deletedProjects);
    // this.isSyncing = false;
    const timeSpent = Date.now() - startTime;
    this.debug && console.log(`Syncing took ${timeSpent} ms.`);
  }

}

