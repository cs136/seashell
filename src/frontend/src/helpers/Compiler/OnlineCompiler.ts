import {SeashellWebsocket, WebsocketResult} from "../Websocket/WebsocketClient";
import {Connection} from "../Services";
import {AbstractCompiler, Test, PID, CompilerResult} from "./Interface";
import {OfflineCompiler} from "./OfflineCompiler";
import {AbstractStorage, ProjectID, FileID} from "../Storage/Interface";

export {OnlineCompiler};

class OnlineCompiler extends AbstractCompiler {

  private socket: SeashellWebsocket;
  private storage: AbstractStorage;
  private offlineCompiler: OfflineCompiler;

  constructor(socket: SeashellWebsocket, storage: AbstractStorage, offComp: OfflineCompiler) {
    super();
    this.socket = socket;
    this.storage = storage;
    this.offlineCompiler = offComp;
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
