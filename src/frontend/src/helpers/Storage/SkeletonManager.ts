import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import {LocalStorage} from "./LocalStorage";
import {ProjectID,
        Project,
        File} from "./Interface";
import * as E from "../Errors";
import * as $ from "jquery";

export {SkeletonManager};

export enum SkeletonStatus { None, Public, Whitelist };

const CS136_URL = "https://www.student.cs.uwaterloo.ca/~cs136/";
const CGI_URL = `${CS136_URL}cgi-bin/`;
const USER_WHITELIST_URL = `${CGI_URL}user_whitelist.cgi`;
const PROJ_WHITELIST_URL = `${CGI_URL}project_whitelist.cgi`;
const PROJ_SKEL_URL = `${CGI_URL}skeleton_list.cgi`;
const SKEL_ROOT_URL = `${CS136_URL}assignment_skeletons/`;
const WL_SKEL_ROOT_URL = "ssh://cs136@linux.student.cs.uwaterloo.ca:/u2/cs136/seashell-support-files/whitelist-skeletons/";
const SKEL_FILE_LIST_URL = `${CGI_URL}skeleton_file_list.rkt`;

class SkeletonManager {

  constructor(private socket: SeashellWebsocket, private storage: LocalStorage) { }

  private userWhitelist: string[];
  private projectWhitelist: string[];
  private projectsWithSkeletons: string[];

  private async getUserWhitelist(): Promise<string[]> {
    if (!this.userWhitelist) {
      try {
        this.userWhitelist = await <PromiseLike<any>>$.get(USER_WHITELIST_URL);
      } catch (e) {
        if (this.socket.isConnected()) {
          throw new E.WebsocketError("Could not load user whitelist file.", e);
        }
        return [];
      }
    }
    return this.userWhitelist || [];
  }

  private async currentUserIsWhitelisted(): Promise<boolean> {
    const users = await this.getUserWhitelist();
    const current = this.socket.getUsername();
    return !!users.find((u: string) => u === current);
  }

  private async getProjectsWithWhitelistSkeletons(): Promise<string[]> {
    if (!this.projectWhitelist) {
      try {
        this.projectWhitelist = await <PromiseLike<any>>$.get(PROJ_WHITELIST_URL);
      } catch (e) {
        if (this.socket.isConnected()) {
          throw new E.WebsocketError("Could not load project whitelist file.", e);
        }
        return [];
      }
    }
    return this.projectWhitelist || [];
  }

  private async getProjectsWithSkeletons(): Promise<string[]> {
    if (!this.projectsWithSkeletons) {
      try {
        this.projectsWithSkeletons = await <PromiseLike<any>>$.get(PROJ_SKEL_URL);
      } catch (e) {
        if (this.socket.isConnected()) {
          throw new E.WebsocketError("Could not load project skeleton list.", e);
        }
        return [];
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
    try {
      const zipURL = await this.getSkeletonZipFileURL(proj);
      if (zipURL) {
        const req = await <PromiseLike<any>>$.get({
          url: SKEL_FILE_LIST_URL,
          data: {
            template: project.name,
            user: this.socket.getUsername(),
            whitelist: (await this._inSkeleton(proj)) === SkeletonStatus.Whitelist
          }
        });
        return req.result.map(
          (path: string) => path.replace(new RegExp(`^${project.name}/`), "")
        ).filter(
          (path: string) => path.length > 0 && path[path.length - 1] !== "/"
        ).sort();
      }
      return [];
    } catch (e) {
      if (this.socket.isConnected()) {
        console.error(e);
        throw new E.SkeletonError("Failed to list project skeleton files.", e);
      }
      return [];
    }
  }

  private async getMissingSkeletonFiles(proj: ProjectID): Promise<string[]> {
    let [localFileObjList, serverFileList] =
      await Promise.all([this.storage.getFiles(proj), this.listSkeletonFiles(proj)]);
    let localFileList = localFileObjList.map((f: File) => f.name);
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
      await Promise.all(missingFiles.map(async (f: string) =>
        this.socket.sendMessage({
          type: "restoreFileFrom",
          project: project.name,
          file: f,
          template: await this.getSkeletonZipFileURL(project.name)
        }).catch((e) => {
          if (!(e instanceof E.NoInternet)) {
            throw e;
          }
        })
      ));
      // sync afterwards to update the local storage.
      // not ideal, but works for now.
      await this.storage.waitForSync(true);
    }
  }

  /* Promise resolves to a list of the new projects that were cloned
      from skeletons. */
  public async fetchNewSkeletons(): Promise<string[]> {
    const localProjects = (await this.storage.getProjects())
      .map((p: Project) => p.name);
    let skels: any = await Promise.all((await this.getProjectsWithSkeletons())
      .map(async (a: string) => [a, await this.getSkeletonZipFileURL(a)]));
    if (await this.currentUserIsWhitelisted()) {
      skels = skels.concat(await Promise.all((await this.getProjectsWithWhitelistSkeletons())
        .map(async (p: string) => [p, await this.getSkeletonZipFileURL(p)])));
    }
    const newProjects = skels.filter((p: string) => -1 === localProjects.indexOf(p[0]));
    let failed: string[] = [];
    console.log("newProjects", newProjects);
    for (let i = 0; i < newProjects.length; i++) {
      try {
        await this.socket.sendMessage({
          type: "newProjectFrom",
          project: newProjects[i][0],
          source: newProjects[i][1]
        });
      } catch (e) {
        if (!(e instanceof E.NoInternet)) {
          console.error(e);
          failed.push(newProjects[i][0]);
        }
      }
    }
    if (failed.length > 0) {
      throw new E.SkeletonError("Failed to fetch new skeletons.", failed);
    }
    return newProjects.map((p: [string, string]) => p[0]);
  }
}
