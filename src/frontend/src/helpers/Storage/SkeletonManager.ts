import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import {LocalStorage} from "./LocalStorage";
import {ProjectID,
        Project,
        File} from "./Interface";
import * as E from "../Errors";

export {SkeletonManager};

/* A project can either:
   - not have a skeleton
   - have a public skeleton
   - have a whitelist skeleton */
export enum SkeletonStatus { None, Public, Whitelist };

const CS136_URL = "https://www.student.cs.uwaterloo.ca/~cs136/";
const CGI_URL = `${CS136_URL}cgi-bin/`;
const USER_WHITELIST_URL = `${CGI_URL}user_whitelist.cgi`;
const PROJ_WHITELIST_URL = `${CGI_URL}project_whitelist.cgi`;
const PROJ_SKEL_URL = `${CGI_URL}skeleton_list.cgi`;
const SKEL_ROOT_URL = `${CS136_URL}assignment_skeletons/`;
const WL_SKEL_ROOT_URL = "ssh://cs136@linux.student.cs.uwaterloo.ca:/u2/cs136/seashell-support-files/whitelist-skeletons/";
const SKEL_FILE_LIST_URL = `${CGI_URL}skeleton_file_list.rkt`;

/* SkeletonManager
  Provides methods for dealing with Seashell skeletons (template projects) */
class SkeletonManager {

  /* SkeletonManager(socket, storage)
    Args:
     socket - the active SeashellWebsocket
     storage - the active LocalStorage */
  constructor(private socket: SeashellWebsocket, private storage: LocalStorage) { }

  private userWhitelist: string[];
  private projectWhitelist: string[];
  private projectsWithSkeletons: string[];

  /* getUserWhiteList()
    Gets the list of "whitelisted" users - those who are able to download instructors-only skeleton projects.

    Returns:
     Array of whitelisted usernames */
  private async getUserWhitelist(): Promise<string[]> {
    if (!this.userWhitelist) {
      try {
        let result = await fetch(USER_WHITELIST_URL);
        if (result.ok) {
          this.userWhitelist = await result.json();
        } else {
          throw new E.SkeletonError("Could not load user whitelist file -- " +  result.statusText);
        }
      } catch (e) {
        if (e instanceof TypeError) {
          return [];
        }
        else {
          throw e;
        }
      }
    }
    return this.userWhitelist || [];
  }

  /* currentUserIsWhitelisted()
    Determines if the current user is a whitelisted user.

    Returns:
     Boolean, true if user is whitelisted, false otherwise. */
  private async currentUserIsWhitelisted(): Promise<boolean> {
    const users = await this.getUserWhitelist();
    const current = this.socket.getUsername();
    return !!users.find((u: string) => u === current);
  }

  /* getProjectsWithWhitelistSkeletons()
    Gets the list of projects that have a whitelist-only skeleton.

    Returns:
     Array of project names */
  private async getProjectsWithWhitelistSkeletons(): Promise<string[]> {
    if (!this.projectWhitelist) {
      try {
        let result = await fetch(PROJ_WHITELIST_URL);
        if (result.ok) {
          this.projectWhitelist = await result.json();
        } else {
          throw new E.SkeletonError("Could not load project whitelist file -- " + result.statusText);
        }
      } catch (e) {
        if (e instanceof TypeError)
          return [];
        else
          throw e;
      }
    }
    return this.projectWhitelist || [];
  }

  /* getProjectsWithSkeletons()
    Gets the list of (non-whitelist) skeleton projects.

    Returns:
     Array of project names */
  private async getProjectsWithSkeletons(): Promise<string[]> {
    if (!this.projectsWithSkeletons) {
      try {
        let result = await fetch(PROJ_SKEL_URL);
        if (result.ok) {
          this.projectsWithSkeletons = await result.json();
        } else {
          throw new E.SkeletonError("Could not load project skeleton file -- " + result.statusText);
        }
      } catch (e) {
        if (e instanceof TypeError) {
          return [];
        } else {
          throw e;
        }
      }
    }
    return this.projectsWithSkeletons || [];
  }

  /* _inSkeleton(pname)
    Determines the SkeletonStatus of a project.

    Args:
     pname - project name
    Returns:
     The SkeletonStatus of that project */
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

  /* inSkeleton(proj)
    Determines whether a project has an associated skeleton.

    Args:
     pid - project ID
    Returns:
     True if project has a skeleton, false otherwise */
  public async inSkeleton(pid: ProjectID): Promise<boolean> {
    const proj = await this.storage.getProject(pid);
    const stat = await this._inSkeleton(proj.name);
    return stat !== SkeletonStatus.None;
  }

  /* listSkeletonFiles(pid)
    Gets a list of files in the skeleton of the given project.

    Args:
     pid - project ID
    Returns:
     Array of filenames in the skeleton */
  private async listSkeletonFiles(pid: ProjectID): Promise<string[]> {
    const project = await this.storage.getProject(pid);
    try {
      const zipURL = await this.getSkeletonZipFileURL(project.name);
      if (zipURL) {
        let querybuilder = new URLSearchParams();
        querybuilder.append("template", project.name);
        querybuilder.append("user", this.socket.getUsername());
        querybuilder.append("whitelist", (await this._inSkeleton(pid)) === SkeletonStatus.Whitelist ? "true" : "false");
        let query = querybuilder.toString();
        let raw = await fetch(`${SKEL_FILE_LIST_URL}?${query}`);
        if (raw.ok) {
          let result = await raw.json();
          if (!result.error) {
            return result.result.map(
              (path: string) => path.replace(new RegExp(`^${project.name}/`), "")
            ).filter(
              (path: string) => path.length > 0 && path[path.length - 1] !== "/"
            ).sort();
          } else {
            throw new E.SkeletonError(`Could not load skeleton files for ${project.name} - ${result.result}.`);
          }
        } else {
          throw new E.SkeletonError(`Could not load skeleton files for ${project.name}.`);
        }
      }
      return [];
    } catch (e) {
      if (e instanceof TypeError) {
        return [];
      }
      else throw e;
    }
  }

  /* getMissingSkeletonFiles(pid)
    Gets a list of files that are present in the skeleton for a project, but missing
    in the user's copy of the project.

    Args:
     pid - project ID
    Returns:
     Array of filenames */
  private async getMissingSkeletonFiles(pid: ProjectID): Promise<string[]> {
    let [localFileObjList, serverFileList] =
      await Promise.all([this.storage.getFiles(pid), this.listSkeletonFiles(pid)]);
    let localFileList = localFileObjList.map((f: File) => f.name);
    return serverFileList.filter((f: string) => !localFileList.find((g: string) => f === g));
  }

  /* getSkeletonZipFileURL(pname)
    Gets the URL of the .zip file for a project's skeleton.

    Args:
     pname - project name
    Returns:
     URL as a string, or false if there is no associated skeleton */
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

  /* pullMissingSkeletonFiles(pid)
    Sends websocket requests to copy all files that are present in the skeleton
    but missing in the user's copy of the project.

    Args:
     pid - project ID */
  public async pullMissingSkeletonFiles(pid: ProjectID): Promise<void> {
    const project = await this.storage.getProject(pid);
    let missingFiles = await this.getMissingSkeletonFiles(pid);
    if (missingFiles.length > 0) {
      await Promise.all(missingFiles.map(async (f: string) =>
        this.socket.sendMessage({
          type: "restoreFileFrom",
          project: pid,
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
      await this.storage.waitForSync();
    }
  }

  /* fetchNewSkeletons()
    Sends websocket requests to clone any skeletons which do not already
    exist in the user's projects.

    Returns:
     Array of project names that were newly cloned. */
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
