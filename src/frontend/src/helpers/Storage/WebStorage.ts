import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import {LocalStorage, ChangeLog} from "./LocalStorage";
import {AbstractStorage, AbstractWebStorage,
        Project, ProjectID, ProjectBrief,
        File, FileID, FileBrief,
        SettingsStored, Settings} from "./Interface";
import {History, Change} from "../types";
import * as E from "../Errors";
export {WebStorage, SeashellWebsocket}
import md5 = require("md5");
import * as R from "ramda";

class WebStorage extends AbstractStorage implements AbstractWebStorage {
  private socket: SeashellWebsocket;
  private storage: LocalStorage;
  private offlineMode: boolean;

  public debug: boolean; // toggle this.debug && console.log

  constructor(wbclient: SeashellWebsocket,
              store: LocalStorage,
              debug?: boolean) {
    super();
    this.storage = store;
    this.socket = wbclient;
    this.debug = debug || false;
  }

  public getOfflineMode(): boolean {
    return this.storage.getOfflineMode();
  }

  public setOfflineMode(enable: boolean): void {
    return this.storage.setOfflineMode(enable);
  }

  public async newFile(pid: ProjectID, filename: string, contents?: string): Promise<FileBrief> {

    // the right way to do it
    // fid pid are md5 strings
    const offline = this.storage.newFile(pid, filename, contents);
    // hack to make it compatible with the backend
    // pid is project name
    const pname = this.getOfflineMode() ? (await this.storage.getProject(pid)).name : pid;
    const online = this.socket.sendMessage<any>({
      type: "newFile",
      project: pname,
      file: filename,
      contents: contents || "",
      encoding: "raw",
      normalize: false
    });

    if (this.getOfflineMode()) {
        return offline;
    } else {
      // hack to make it compatible with the backend
      // fid is project/question/filename
      await online;
      return new FileBrief({
        id: `${pid}/filename`,
        name: filename,
        last_modified: Date.now(),
        project: pname,
        open: false,
        checksum: md5(contents || ""),
        contents: undefined
      });
    }

  };

  public async renameFile(fid: FileID, newName: string): Promise<void> {
    // the right way to do it
    // fid pid are md5 strings
    let fname: string;
    let pname: string;
    if (this.getOfflineMode()) {
      const file = await this.storage.readFile(fid);
      fname = file.name;
      pname = (await this.storage.getProject(file.project)).name;
    } else {
      // hack to make it compatible with the backend
      // fid is project/question/filename
      // pid is project name
      const split = R.split("/", fid);
      pname = split[0];
      fname = `${split[1]}/${split[2]}`;
    }
    const offline = this.storage.renameFile(fid, newName);
    const online  = this.socket.sendMessage<void>({
      type: "renameFile",
      project: pname,
      oldName: fname,
      newName: newName
    });
    return await (this.getOfflineMode() ? offline : online);
  };

  public async deleteFile(fid: FileID): Promise<void> {
    // the right way to do it
    // fid pid are md5 strings
    let fname: string;
    let pname: string;
    if (this.getOfflineMode()) {
      const file = await this.storage.readFile(fid);
      fname = file.name;
      pname = (await this.storage.getProject(file.project)).name;
    } else {
      // hack to make it compatible with the backend
      // fid is project/question/filename
      // pid is project name
      const split = R.split("/", fid);
      pname = split[0];
      fname = `${split[1]}/${split[2]}`;
    }
    const offline = this.storage.deleteFile(fid);
    const online  = this.socket.sendMessage<void>({
      type: "deleteFile",
      project: pname,
      file: fname
    });
    return await (this.getOfflineMode() ? offline : online);
  };

  public async deleteProject(pid: ProjectID): Promise<void> {
    // the right way to do it
    // pid is md5 string
    // hack to make it compatible with the backend
    // pid is project name
    const pname = this.getOfflineMode() ? (await this.storage.getProject(pid)).name : pid;
    const offline = this.storage.deleteProject(pid);
    const online  = this.socket.sendMessage<void>({
      type: "deleteProject",
      project: pname
    });
    return await (this.getOfflineMode() ? offline : online);
  };

  public async newProject(name: string): Promise<ProjectBrief> {
    const offline = this.storage.newProject(name);
    const online  = this.socket.sendMessage({
      type: "newProject",
      project: name
    });
    if (this.getOfflineMode()) {
      // sitll needs to wait for the backend
      // sending multiple requests at once breaks the backend
      await this.ignoreNoInternet(online);
      return offline;
    } else {
      // hack to make it compatible with the backend
      // pid is project name
      await online;
      return new ProjectBrief({
        id: name,
        name: name,
        last_modified: Date.now(),
        runs: {}
      });
    }
  }

  public async getProjects(): Promise<ProjectBrief[]> {
    if (this.getOfflineMode()) {
      return this.storage.getProjects();
    }
    // hack to make it compatible with the backend
    // pid is project name
    const result = await this.socket.sendMessage<[string, number][]>({
      type: "getProjects"
    });
    return R.map((data: [string, number]) => (new ProjectBrief({
      id: data[0],
      name: data[0],
      last_modified: data[1],
      runs: {}
    })), result);
  }

  public async getProject(pid: ProjectID): Promise<Project> {
    if (this.getOfflineMode()) {
      return this.storage.getProject(pid);
    }
    // hack to make it compatible with the backend
    // pid is project name
    return new ProjectBrief({
      id: pid,
      name: pid,
      last_modified: Date.now(),
      runs: {}
    });
  }

  public async readFile(fid: FileID): Promise<File> {
    if (this.getOfflineMode()) {
      // the right way to do it
      // fid is md5 string
      return this.storage.readFile(fid);
    }
    // hack to make it compatible with the backend
    // fid is project/question/file
    const split = R.split("/", fid);
    const pname = split[0];
    const fname = `${split[1]}/${split[2]}`;
    const result = await this.socket.sendMessage<any>({
      type: "readFile",
      project: pname,
      file: fname
    });
    return new File({
      id: fid,
      name: fname,
      project: pname,
      last_modified: Date.now(),
      checksum: result.checksum,
      contents: result.data,
      open: false
    });
  }

  public async writeFile(fid: FileID, contents: string): Promise<void> {
    let pname: string;
    let fname: string;
    if (this.getOfflineMode()) {
      // the right way to do it
      // fid is md5 string
      const file = await this.storage.readFile(fid);
      pname = (await this.storage.getProject(file.project)).name;
      fname = file.name;
    } else {
      // hack to make it compatible with the backend
      // fid is project/question/file
      const split = R.split("/", fid);
      pname = split[0];
      fname = split[1];
    }
    const offline = this.storage.writeFile(fid, contents);
    const online  = this.socket.sendMessage<void>({
      type: "writeFile",
      file: fname,
      project: pname,
      contents: contents,
      history: ""
    });
    return await (this.getOfflineMode() ? offline : online);
  }

  // public async getTestResults(marmosetProject: string): Promise<any> {
  //   await this.socket.sendMessage({
  //     type: "marmosetTestResults",
  //     project: marmosetProject,
  //     testtype: "public"
  //   });
  // }

  public async getProjectFiles(pid: ProjectID): Promise<FileBrief[]> {
    if (this.getOfflineMode()) {
      // the right way to do it
      // pid is md5 string
      return this.storage.getProjectFiles(pid);
    } else {
      // hack to make it compatible with the backend
      // pid is project name
      const result = await this.socket.sendMessage<any>({
        type: "listProject",
        project: pid,
      });
      return R.map((data: [string, boolean, number, string]) => new FileBrief({
        id: `${pid}/${data[0]}`, // use project/question/filename
        name: data[0],
        last_modified: data[2],
        checksum: data[3],
        project: pid,
        open: false,
        contents: undefined
      }), result);
    }
  };

  public async getAllFiles(): Promise<FileBrief[]> {
    // getAllFiles is designed to be used in syncAll
    // offline mode must be enabled
    console.assert(this.getOfflineMode());
    return this.storage.getAllFiles();
  };

  public async getFileToRun(pid: ProjectID, question: string): Promise<string|false> {
    if (this.getOfflineMode()) {
      // the right way to do it
      // pid is md5 string
      return this.storage.getFileToRun(pid, question);
    } else {
      // hack to make it compatible with the backend
      // pid is project name
      try {
        return await this.socket.sendMessage<string|false>({
          type: "getFileToRun",
          project: pid,
          question: question
        });
      } catch (err) {
        if (err instanceof E.RequestError) {
          return false;
        }
        throw err;
      }
    }
  };

  public async setFileToRun(proj: ProjectID, question: string, filename: string): Promise<void> {
    // the right way to do it
    // pid is md5 string
    // hack to make it compatible with the backend
    // pid is project name
    const pjName = this.getOfflineMode() ? (await this.storage.getProject(proj)).name : proj;
    const offline = this.storage.setFileToRun(proj, question, filename);
    const arr = filename.split("/");
    const folder = arr.length > 2 ? "tests" : arr[0] === "common" ? "common" : "question";
    const online  = this.socket.sendMessage<void>({
      type: "setFileToRun",
      project: pjName,
      question: question,
      file: filename,
      folder: folder
    });
    return await (this.getOfflineMode() ? offline : online);
  };

  public async getOpenTabs(proj: ProjectID, question: string): Promise<FileBrief[]> {
    // if (this.getOfflineMode()) {
      return this.storage.getOpenTabs(proj, question);
    // }
    // not done
  }

  public async addOpenTab(proj: ProjectID, question: string, fid: FileID): Promise<void> {
    // if (this.getOfflineMode()) {
      return this.storage.addOpenTab(proj, question, fid);
    // }
    // not done
  }

  public async removeOpenTab(proj: ProjectID, question: string, fid: FileID): Promise<void> {
    // if (this.getOfflineMode()) {
      return this.storage.removeOpenTab(proj, question, fid);
    // }
    // not done
  }

  public async setSettings(settings: Settings): Promise<void> {
    const offline = this.storage.setSettings(settings);
    const online  = this.socket.sendMessage<void>({
      type: "saveSettings",
      settings: settings.toJSON()
    });
    return await (this.getOfflineMode() ? offline : online);
  }

  public async getSettings(): Promise<Settings> {
    if (this.getOfflineMode()) {
      return this.storage.getSettings();
    } else {
      const json = await this.socket.sendMessage({
        type: "getSettings"
      }) as SettingsStored;
      return Settings.fromJSON(json);
    }
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

  private async ignoreNoInternet<T>(promise: Promise<T>): Promise<void> {
    try {
      await promise;
    } catch (err) {
      if (err instanceof E.NoInternet) {
        throw err;
      } else {
        console.warn("No internet. Changes were written to indexedDB only.");
        return;
      }
    }
  }

}

