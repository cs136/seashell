import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import {LocalStorage, ChangeLog} from "./LocalStorage";
import {AbstractStorage, AbstractWebStorage,
        Project, ProjectID, ProjectBrief,
        File, FileID, FileBrief,
        Settings} from "./Interface";
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

  public debug: boolean; // toggle this.debug && console.log

  constructor(wbclient: SeashellWebsocket, store: LocalStorage, debug?: boolean) {
    super();
    this.storage = store;
    this.socket = wbclient;
    this.debug = debug || false;
  }

  public async newFile(pid: ProjectID, filename: string, contents?: string): Promise<FileBrief> {
    const fid = await this.storage.newFile(pid, filename, contents);
    const proj = await this.storage.getProject(pid);
    await this.socket.sendMessage({
      type: "newFile",
      project: proj.name,
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

  public async newProject(name: string): Promise<ProjectBrief> {
    const pid = await this.storage.newProject(name);
    await this.socket.sendMessage({
      type: "newProject",
      project: name
    });
    return pid;
  }

  public async getProjects(): Promise<ProjectBrief[]> {
    // await this.syncAll();
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
    const proj = await this.storage.getProject(file.project);
    await this.storage.writeFile(fid, contents);
    await this.socket.sendMessage({
      type: "writeFile",
      file: file.name,
      project: proj.name,
      contents: contents,
      history: ""
    });
  }

  public async getTestResults(marmosetProject: string): Promise<any> {
    await this.socket.sendMessage({
      type: "marmosetTestResults",
      project: marmosetProject,
      testtype: "public"
    });
  }

  public async getProjectFiles(pid: ProjectID): Promise<FileBrief[]> {
    // await this.syncAll();
    return this.storage.getProjectFiles(pid);
  };

  public async getAllFiles(): Promise<FileBrief[]> {
    // await this.syncAll();
    return this.storage.getAllFiles();
  };

  public async getFileToRun(proj: ProjectID, question: string): Promise<string|false> {
    return this.storage.getFileToRun(proj, question);
  };

  public async setFileToRun(proj: ProjectID, question: string, filename: string): Promise<void> {
    return this.storage.setFileToRun(proj, question, filename);
  };

  public async getOpenTabs(proj: ProjectID, question: string): Promise<FileBrief[]> {
    return this.storage.getOpenTabs(proj, question);
  }

  public async addOpenTab(proj: ProjectID, question: string, fid: FileID): Promise<void> {
    return this.storage.addOpenTab(proj, question, fid);
  }

  public async removeOpenTab(proj: ProjectID, question: string, fid: FileID): Promise<void> {
    return this.storage.removeOpenTab(proj, question, fid);
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
    let startTime = Date.now();
    let frontSpent = 0;
    let backSpent = 0;
    const projectsSent = await this.storage.getProjects();
    const filesSent = await this.storage.getAllFiles();
    // create a cache to reduce getProject calls
    // it's a bit hacky but ideally the backend should recongize project IDs directly
    // so we don't need to call getProject here at all to find the project name
    const projectIDNameMap: {[index: string]: string} = {};
    for (const file of filesSent) {
      (<any>file).file = file.name;
      if (! projectIDNameMap[file.project]) {
        projectIDNameMap[file.project] = (await this.storage.getProject(file.project)).name;
      }
      (<any>file).project = projectIDNameMap[file.project];
    }
    const changesSent = await this.storage.getChangeLogs();
    const settingsSent = await this.storage.getSettings();
    frontSpent += Date.now() - startTime;
    startTime = Date.now();
    const result = await this.socket.sendMessage<{
      changes: ChangeLog[],
      newProjects: string[],
      deletedProjects: string[]
    }>({
      type: "sync",
      projects: projectsSent,
      files: filesSent,
      changes: changesSent,
      settings: {
        modified: Date.now(),
        values: JSON.stringify(settingsSent),
        name: "settings"
      }
    });
    backSpent += Date.now() - startTime;
    startTime = Date.now();
    const changesGot = result.changes;
    const newProjects: string[] = result.newProjects;
    const deletedProjects: ProjectID[] = R.map<string, string>(md5, result.deletedProjects);
    await this.storage.applyChanges(changesGot, newProjects, deletedProjects);
    frontSpent += Date.now() - startTime;
    this.debug && console.log(`Syncing took ${frontSpent + backSpent} ms. Frontend took ${frontSpent} ms. Backend took ${backSpent} ms.`);
  }

}

