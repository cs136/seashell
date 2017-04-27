import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import {WebStorage} from "./WebStorage";
import {ProjectID,
        FileBrief} from "./Interface";
import * as E from "../Errors";
import * as $ from "jquery";

export {SkeletonManager};

enum SkeletonStatus { None, Public, Whitelist };

const USER_WHITELIST_URL = "";
const PROJ_WHITELIST_URL = "";
const PROJ_SKEL_URL = "";
const SKEL_URL = "";

class SkeletonManager {

  constructor(private socket: SeashellWebsocket, private storage: WebStorage) { }

  private userWhitelist: string[];
  private projectWhitelist: string[];
  private projectsWithSkeletons: string[];

  // The following functions implement the project skeleton feature

  private async getUserWhitelist(): Promise<string[]> {
    if (!this.userWhitelist) {
      try {
        this.userWhitelist = (await <PromiseLike<any>>$.get(USER_WHITELIST_URL)).data;
      } catch (e) {
        throw new E.WebsocketError("Could not load user whitelist file.", e);
      }
    }
    return this.userWhitelist || [];
  }

  private async getProjectWhitelist(): Promise<string[]> {
    if (!this.projectWhitelist) {
      try {
        this.projectWhitelist = (await <PromiseLike<any>>$.get(PROJ_WHITELIST_URL)).data;
      } catch (e) {
        throw new E.WebsocketError("Could not load project whitelist file.", e);
      }
    }
    return this.projectWhitelist || [];
  }

  private async getProjectsWithSkeletons(): Promise<string[]> {
    if (!this.projectsWithSkeletons) {
      try {
        this.projectsWithSkeletons = (await <PromiseLike<any>>$.get(PROJ_SKEL_URL)).data;
      } catch (e) {
        throw new E.WebsocketError("Could not load project skeleton list.", e);
      }
    }
    return this.projectsWithSkeletons || [];
  }

  public async inSkeleton(proj: ProjectID): Promise<SkeletonStatus> {
    let [project, names, users, wlnames] =
      await Promise.all([this.storage.getProject(proj), this.getProjectsWithSkeletons(),
        this.getUserWhitelist(), this.getProjectWhitelist()]);
    let ans = SkeletonStatus.None;
    const user = this.socket.getUsername();
    if (names.find((a: string) => a === project.name)) ans = SkeletonStatus.Public;
    else if (users.find((a: string) => a === user)
        && wlnames.find((a: string) => a === project.name)) {
      ans = SkeletonStatus.Whitelist;
    }
    return ans;
  }

  private async listSkeletonFiles(proj: ProjectID, user: string): Promise<string[]> {
    const project = await this.storage.getProject(proj);
    return (await <PromiseLike<any>>$.get({
      url: SKEL_URL,
      data: {
        user: user,
        whitelist: "true"
      }
    })).data.result.map(
      (path: string) => path.replace(new RegExp(`^${project.name}/`), "")
    ).filter(
      (path: string) => path.length > 0 && path[path.length - 1] !== "/"
    ).sort();
  }

  private async getMissingSkeletonFiles(proj: ProjectID, user: string): Promise<string[]> {
    let [localFileBriefList, serverFileList] =
      await Promise.all([this.storage.getProjectFiles(proj), this.listSkeletonFiles(proj, user)]);
    let localFileList = localFileBriefList.map((f: FileBrief) => f.name);
    return serverFileList.filter((f: string) => localFileList.find((g: string) => f === g));
  }

  private getProjectSkeletonZipFile(pname: string): string {
    return "";
  }

  public async pullMissingSkeletonFiles(proj: ProjectID, user: string): Promise<void> {
    const project = await this.storage.getProject(proj);
    let missingFiles = await this.getMissingSkeletonFiles(proj, user);
    if (missingFiles.length > 0) {
      await Promise.all(missingFiles.map((f: string) =>
        this.socket.sendMessage({
          type: "restoreFileFrom",
          project: project.name,
          file: f,
          template: this.getProjectSkeletonZipFile(project.name)
        })
      ));
      // sync afterwards to update the local storage.
      // not ideal, but just for now.
      return this.storage.syncAll();
    }
  }
}
