import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import {LocalStorage, ChangeLog} from "./LocalStorage";
import {SkeletonManager} from "./SkeletonManager";
import {AbstractStorage, AbstractWebStorage,
        Project, ProjectID, ProjectBrief,
        File, FileID, FileBrief,
        SettingsStored, Settings,
        OfflineMode} from "./Interface";
import {History, Change} from "../types";
import * as E from "../Errors";
export {WebStorage}
import md5 = require("md5");
import * as R from "ramda";

enum FileCategory { Common, Test, Directory, Other };

class WebStorage extends AbstractStorage implements AbstractWebStorage {

  constructor(private socket: SeashellWebsocket,
              private storage: LocalStorage,
              private offlineMode: OfflineMode = OfflineMode.On,
              public debug: boolean = false) {
    super();
    this.skeletons = new SkeletonManager(socket, this);
  }

  private skeletons: SkeletonManager;

  public async newFile(pid: ProjectID, filename: string, contents?: string): Promise<FileBrief> {
    // the right way to do it
    // fid pid are md5 strings
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      return this.storage.newFile(pid, filename, contents);
    }
    // hack to make it compatible with the backend
    // pid is project name
    const id = this.storage.projID(pid);
    await this.socket.sendMessage<any>({
      type: "newFile",
      project: pid,
      file: filename,
      contents: contents || "",
      encoding: "raw",
      normalize: false
    });
    return new FileBrief({
      id: `${pid}/${filename}`,
      name: filename,
      last_modified: Date.now(),
      project: pid,
      open: false,
      checksum: md5(contents || ""),
      contents: undefined
    });
  };

  public async renameFile(fid: FileID, newName: string): Promise<FileBrief> {
    // the right way to do it
    // fid pid are md5 strings
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      return this.storage.renameFile(fid, newName);
    }
    // hack to make it compatible with the backend
    // fid is project/question/filename
    // pid is project name
    const split = R.split("/", fid);
    const pname = split[0];
    const fname = R.join("/", R.drop(1, split));
    await this.socket.sendMessage<void>({
      type: "renameFile",
      project: pname,
      oldName: fname,
      newName: newName
    });
    const file = await this.readFile(`${pname}/${newName}`);
    return new FileBrief(file);
  };

  public async deleteFile(fid: FileID): Promise<void> {
    // the right way to do it
    // fid pid are md5 strings
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      return this.storage.deleteFile(fid);
    }
    // hack to make it compatible with the backend
    // fid is project/question/filename
    // pid is project name
    const split = R.split("/", fid);
    const pname = split[0];
    const fname = R.join("/", R.drop(1, split));
    const pid = this.storage.projID(pname);
    const id = this.storage.fileID(pid, fname);
    const offline = this.storage.deleteFile(id);
    return this.socket.sendMessage<void>({
      type: "deleteFile",
      project: pname,
      file: fname
    });
  };

  public async deleteProject(pid: ProjectID): Promise<void> {
    // the right way to do it
    // pid is md5 string
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      return this.storage.deleteProject(pid);
    }
    // hack to make it compatible with the backend
    // pid is project name
    return this.socket.sendMessage<void>({
      type: "deleteProject",
      project: pid
    });
  };

  public async newProject(name: string): Promise<ProjectBrief> {
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      return this.storage.newProject(name);
    }
    // hack to make it compatible with the backend
    // pid is project name
    await this.socket.sendMessage({
      type: "newProject",
      project: name
    });
    return new ProjectBrief({
      id: name,
      name: name,
      last_modified: Date.now(),
      settings: {}
    });
  }

  public async getProjects(): Promise<ProjectBrief[]> {
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
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
      settings: {}
    })), result);
  }

  public async getProject(pid: ProjectID): Promise<Project> {
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      return this.storage.getProject(pid);
    }
    // hack to make it compatible with the backend
    // pid is project name
    return new ProjectBrief({
      id: pid,
      name: pid,
      last_modified: Date.now(),
      settings: {}
    });
  }

  public async readFile(fid: FileID): Promise<File> {
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      // the right way to do it
      // fid is md5 string
      return this.storage.readFile(fid);
    }
    // hack to make it compatible with the backend
    // fid is project/question/file
    const split = R.split("/", fid);
    const pname = split[0];
    const fname = R.join("/", R.drop(1, split));
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
    // the right way to do it
    // fid is md5 string
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      return this.storage.writeFile(fid, contents);
    }
    // hack to make it compatible with the backend
    // fid is project/question/file
    const split = R.split("/", fid);
    const pname = split[0];
    const fname = R.join("/", R.drop(1, split));
    await this.socket.sendMessage<void>({
      type: "writeFile",
      file: fname,
      project: pname,
      contents: contents,
      history: ""
    });
  }

  public async marmosetSubmit(project_name: string, marmosetProject: string, question: string) {
    await this.socket.sendMessage({
      type: "marmosetSubmit",
      project: project_name,
      subdir: question,
      assn: marmosetProject
    });
  }

  public async getTestResults(marmosetProject: string): Promise<any> {
    return this.socket.sendMessage({
      type: "marmosetTestResults",
      project: marmosetProject,
      testtype: "public"
    });
  }

  // private categorizeFile(fname: string): FileCategory {
  //   /* fname:
  //    common
  //    common/[filename]
  //    [question]
  //    [question]/[filename]
  //    [question]/tests
  //    [question]/tests/[filename]
  //   */
  //   const regxTest = /^[^\/]+(\/tests)(\/[^\/]+)$/;
  //   const regxCommon = /^common(\/[^\/]+)$/;
  //   const regxDir = /^[^\/]+(\/tests)?$/;
  //   if (R.test(regxTest, fname)) {
  //     return FileCategory.Test;
  //   }
  //   if (R.test(regxCommon, fname)) {
  //     return FileCategory.Common;
  //   }
  //   if (R.test(regxDir, fname)) {
  //     return FileCategory.Directory;
  //   }
  //   return FileCategory.Other;
  // }

  public async getProjectFiles(pid: ProjectID): Promise<FileBrief[]> {
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      // the right way to do it
      // pid is md5 string
      return this.storage.getProjectFiles(pid);
    }
    // hack to make it compatible with the backend
    // pid is project name
    const result = await this.socket.sendMessage<[string, boolean, number, string][]>({
      type: "listProject",
      project: pid,
    });
    return R.reduce((acc, data) => {
      // the backend return fname as [question](/(common|tests))?(/[filename])?
      const fname: string = data[0];
      // const cat = this.categorizeFile(fname);
      // data[1]: is directory?
      if (data[1]) {
        return acc;
      } else {
        return R.append(new FileBrief({
          id: `${pid}/${fname}`, // use project/question(/(common|tests))?/filename
          name: data[0],
          last_modified: data[2],
          checksum: data[3],
          project: pid,
          open: false,
          contents: undefined
        }), acc);
      }
    }, [], result);
  };

  public async getAllFiles(): Promise<FileBrief[]> {
    // getAllFiles is designed to be used in syncAll
    // offline mode must be enabled
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      return this.storage.getAllFiles();
    }
    // the backend should support getAllFiles it current doesn't
    // as a result this part is only used in test
    const pjs = await this.getProjects();
    const fbs = R.map((p) => this.getProjectFiles(p.id), pjs);
    return R.flatten(await Promise.all(fbs));
  };

  public async getFileToRun(pid: ProjectID, question: string): Promise<string|false> {
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      // the right way to do it
      // pid is md5 string
      return this.storage.getFileToRun(pid, question);
    }
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
  };

  public async setFileToRun(pid: ProjectID, question: string, filename: string): Promise<void> {
    // the right way to do it
    // pid is md5 string
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      return this.storage.setFileToRun(pid, question, filename);
    }
    // hack to make it compatible with the backend
    // pid is project name
    const arr = filename.split("/");
    const folder = arr.length > 2 ? "tests" : arr[0] === "common" ? "common" : "question";
    return this.socket.sendMessage<void>({
      type: "setFileToRun",
      project: pid,
      question: question,
      file: R.join("/", R.drop(1, arr)),
      folder: folder
    });
  };

  public async getOpenFiles(pid: ProjectID, question: string): Promise<FileBrief[]> {
    // if (this.offlineMode === OfflineMode.On) {
      return this.storage.getOpenFiles(pid, question);
    // }
    // not done
  }

  public async addOpenFile(pid: ProjectID, question: string, fid: FileID): Promise<void> {
    // if (this.offlineMode === OfflineMode.On) {
      return this.storage.addOpenFile(pid, question, fid);
    // }
    // not done
  }

  public async removeOpenFile(pid: ProjectID, question: string, fid: FileID): Promise<void> {
    // if (this.offlineMode === OfflineMode.On) {
      return this.storage.removeOpenFile(pid, question, fid);
    // }
    // not done
  }

  public async setSettings(settings: Settings): Promise<void> {
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      return this.storage.setSettings(settings);
    }
    return this.socket.sendMessage<void>({
      type: "saveSettings",
      settings: settings.toJSON()
    });
  }

  public async getSettings(): Promise<Settings> {
    if (this.offlineMode === OfflineMode.On ||
        this.offlineMode === OfflineMode.Forced) {
      return this.storage.getSettings();
    }
    const json = await this.socket.sendMessage({
      type: "getSettings"
    }) as SettingsStored;
    return Settings.fromJSON(json);
  }

  // to be replaced by dexie
  public async syncAll(fetchServerChanges: boolean = true): Promise<void> {
    if (this.offlineMode === OfflineMode.Off) {
      console.log("Ignoring sync request --- offline mode disabled.");
      return;
    }
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
    if (!fetchServerChanges) {
      if (changesSent.length === 0) {
        console.log("Ignoring sync request --- no local file changes made.");
        return;
      }
    }
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
    const deletedProjects: ProjectID[] = R.map<string, string>(this.storage.projID, result.deletedProjects);
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

  public async inSkeleton(proj: ProjectID): Promise<boolean> {
    return this.skeletons.inSkeleton(proj);
  }

  public async pullMissingSkeletonFiles(proj: ProjectID): Promise<void> {
    return this.skeletons.pullMissingSkeletonFiles(proj);
  }

  public async fetchNewSkeletons(): Promise<string[]> {
    return this.skeletons.fetchNewSkeletons();
  }
}
