import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import {LocalStorage} from "./LocalStorage";
import {Connection} from "../Websocket/Interface";
import {SkeletonManager} from "./SkeletonManager";
import {Project, ProjectID,
        File, FileID,
        SettingsStored, Settings,
        OfflineMode} from "./Interface";
import {History, Change} from "../types";
import * as E from "../Errors";
import md5 = require("md5");
import * as R from "ramda";
import * as $ from "jquery";

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
    await this.socket.sendMessage({
      type: "marmosetSubmit",
      project: project,
      subdir: question,
      assn: marmosetProject
    }).catch((e) => {
      if (!(e instanceof E.NoInternet)) {
        throw e;
      }
    });
  }

  public async getTestResults(marmosetProject: string): Promise<any> {
    return this.socket.sendMessage({
      type: "marmosetTestResults",
      project: marmosetProject,
      testtype: "public"
    }).catch((e) => {
      if (!(e instanceof E.NoInternet)) {
        throw e;
      }
      return {};
    });
  }

  public async getMarmosetProjects(): Promise<MarmosetProject[]> {
    return new Promise<MarmosetProject[]>((acc, rej) => {
      try {
        $.get("https://www.student.cs.uwaterloo.ca/~cs136/cgi-bin/marmoset-utils/project-list.rkt",
          (lst) => acc(lst));
      } catch (e) {
        if (this.socket.isConnected()) {
          rej(e);
        }
        return [];
      }
    });
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
    await this.socket.sendMessage({
      type: "archiveProjects",
      location: false
    }).catch((e) => {
      if (!(e instanceof E.NoInternet)) {
        throw e;
      }
    });
  }
}
