import {SeashellWebsocket, WebsocketResult} from "../WebsocketClient";
import {Connection} from "../Login";
import {AbstractCompiler, Test, PID, CompilerResult} from "./Interface";
import {OfflineCompiler} from "./OfflineCompiler";
import {ProjectID, FileID} from "../Storage";

export {OnlineCompiler};

class OnlineCompiler extends AbstractCompiler {

  private offlineCompiler: OfflineCompiler;
  private socket: SeashellWebsocket;

  constructor(offComp: OfflineCompiler) {
    super();
    this.offlineCompiler = offComp;
    this.socket = SeashellWebsocket.getInstance();
  }
  
  public async compileAndRunProject(proj: ProjectID, question: string, file: FileID, tests: Test[]): Promise<CompilerResult> {
    return {
      messages: [],
      pid: null,
      status: "failed"
    };
  }

  public async programInput(pid: PID, contents: string): Promise<void> {
  }

  public async startIO(project: ProjectID, pid: PID): Promise<void> {
  }
}
