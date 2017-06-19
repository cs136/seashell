import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import {LocalStorage} from "./LocalStorage";
import {SkeletonManager} from "./SkeletonManager";
import {Project, ProjectID, ProjectBrief,
        File, FileID, FileBrief,
        SettingsStored, Settings,
        OfflineMode} from "./Interface";
import {History, Change} from "../types";
import * as E from "../Errors";
export {WebStorage}
import md5 = require("md5");
import * as R from "ramda";

enum FileCategory { Common, Test, Directory, Other };

class WebStorage {

  constructor(private socket: SeashellWebsocket,
              private localStorage: LocalStorage,
              public debug: boolean = false) {
    this.skeletons = new SkeletonManager(socket, localStorage);
  }

  private skeletons: SkeletonManager;

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
