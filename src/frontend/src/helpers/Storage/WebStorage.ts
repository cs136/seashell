import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import {LocalStorage} from "./LocalStorage";
import {Connection} from "../Websocket/Interface";
import {SkeletonManager} from "./SkeletonManager";
import {Project, ProjectID,
        File, FileID,
        SettingsStored, Settings} from "./Interface";
import {History, Change} from "../types";
import * as E from "../Errors";
import * as R from "ramda";

export {WebStorage, MarmosetProject}

enum FileCategory { Common, Test, Directory, Other };

interface MarmosetProject {
  project: string;
  title: string;
}

class WebStorage {

  constructor(private socket: SeashellWebsocket,
              private localStorage: LocalStorage,
              public debug: boolean = false) {
    this.skeletons = new SkeletonManager(socket, localStorage);
  }

  private skeletons: SkeletonManager;

  public async marmosetSubmit(project: ProjectID, question: string, marmosetProject: string) {
    await this.localStorage.waitForSync();
    return await this.socket.sendMessage({
      type: "marmosetSubmit",
      project: project,
      subdir: question,
      assn: marmosetProject
    });
  }

  public async getTestResults(marmosetProject: string): Promise<any> {
    return await this.socket.sendMessage({
      type: "marmosetTestResults",
      project: marmosetProject,
      testtype: "public"
    });
  }

  public async getMarmosetProjects(): Promise<MarmosetProject[]> {
    let result: MarmosetProject[] = [];
    try {
      let raw = await fetch("https://www.student.cs.uwaterloo.ca/~cs136/cgi-bin/marmoset-utils/project-list.rkt");

      if (raw.ok) {
        return await raw.json();
      } else {
        throw new Error("Could not list Marmoset projects -- " + raw.statusText);
      }
    } catch (e) {
      if (e instanceof TypeError) {
        return [];
      }
      throw e;
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

  public async archiveProjects(): Promise<void> {
    await this.localStorage.waitForSync();
    let result = await this.socket.sendMessage({
      type: "archiveProjects",
      location: false
    });
    await this.localStorage.waitForSync();
    return;
  }
}
