import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import {WebStorage} from "./WebStorage";
import {ProjectID,
        ProjectBrief,
        FileBrief} from "./Interface";
import * as E from "../Errors";
import * as $ from "jquery";

export {SkeletonManager};

export enum SkeletonStatus { None, Public, Whitelist };

const CS136_URL = "https://www.student.cs.uwaterloo.ca/~cs136/";
const CGI_URL = `${CS136_URL}cgi-bin/`;
const USER_WHITELIST_URL = "";
const PROJ_WHITELIST_URL = "";
const PROJ_SKEL_URL = `${CGI_URL}skeleton_list.cgi`;
const SKEL_ROOT_URL = `${CS136_URL}assignment_skeletons/`;
const WL_SKEL_ROOT_URL = "ssh://cs136@linux.student.cs.uwaterloo.ca:/u2/cs136/seashell-support-files/whitelist-skeletons/";
const SKEL_FILE_LIST_URL = `${CGI_URL}skeleton_file_list.rkt`;

class SkeletonManager {

  constructor(private socket: SeashellWebsocket, private storage: WebStorage) { }

  private userWhitelist: string[];
  private projectWhitelist: string[];
  private projectsWithSkeletons: string[];

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

  private async getProjectsWithWhitelistSkeletons(): Promise<string[]> {
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

  private async _inSkeleton(pname: string): Promise<SkeletonStatus> {
    let [names, users, wlnames] =
      await Promise.all([this.getProjectsWithSkeletons(),
        this.getUserWhitelist(), this.getProjectsWithWhitelistSkeletons()]);
    let ans = SkeletonStatus.None;
    const user = this.socket.getUsername();
    if (names.find((a: string) => a === pname)) ans = SkeletonStatus.Public;
    else if (users.find((a: string) => a === user)
        && wlnames.find((a: string) => a === pname)) {
      ans = SkeletonStatus.Whitelist;
    }
    return ans;
  }

  public async inSkeleton(proj: ProjectID): Promise<boolean> {
    const stat = await this._inSkeleton(proj);
    return stat !== SkeletonStatus.None;
  }

  private async listSkeletonFiles(proj: ProjectID): Promise<string[]> {
    const project = await this.storage.getProject(proj);
    return (await <PromiseLike<any>>$.get({
      url: SKEL_FILE_LIST_URL,
      data: {
        template: await this.getSkeletonZipFileURL(proj),
        user: this.socket.getUsername(),
        whitelist: (await this._inSkeleton(proj)) === SkeletonStatus.Whitelist
      }
    })).data.result.map(
      (path: string) => path.replace(new RegExp(`^${project.name}/`), "")
    ).filter(
      (path: string) => path.length > 0 && path[path.length - 1] !== "/"
    ).sort();
  }

  private async getMissingSkeletonFiles(proj: ProjectID): Promise<string[]> {
    let [localFileBriefList, serverFileList] =
      await Promise.all([this.storage.getProjectFiles(proj), this.listSkeletonFiles(proj)]);
    let localFileList = localFileBriefList.map((f: FileBrief) => f.name);
    return serverFileList.filter((f: string) => localFileList.find((g: string) => f === g));
  }

  private async getSkeletonZipFileURL(pname: string): Promise<string|false> {
    const stat = await this._inSkeleton(pname);
    if (stat === SkeletonStatus.Public) {
      return `${SKEL_ROOT_URL}${pname}-seashell.zip`;
    } else if (stat === SkeletonStatus.Whitelist) {
      return `${WL_SKEL_ROOT_URL}${pname}-seashell.zip`;
    } else {
      return false;
    }
  }

  public async pullMissingSkeletonFiles(proj: ProjectID): Promise<void> {
    const project = await this.storage.getProject(proj);
    let missingFiles = await this.getMissingSkeletonFiles(proj);
    if (missingFiles.length > 0) {
      await Promise.all(missingFiles.map((f: string) =>
        this.socket.sendMessage({
          type: "restoreFileFrom",
          project: project.name,
          file: f,
          template: this.getSkeletonZipFileURL(project.name)
        })
      ));
      // sync afterwards to update the local storage.
      // not ideal, but works for now.
      return this.storage.syncAll();
    }
  }

  public async fetchNewSkeletons(): Promise<void> {
    const localProjects = (await this.storage.getProjects())
      .map((p: ProjectBrief) => p.name);
    const skels = await Promise.all((await this.getProjectsWithSkeletons())
      .map(async (a: string) => [a, await this.getSkeletonZipFileURL(a)]));
  }
}
