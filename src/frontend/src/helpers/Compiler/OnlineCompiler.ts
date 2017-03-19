import {SeashellWebsocket,
        WebsocketResult} from "../Websocket/WebsocketClient";
import {Connection} from "../Services";
import {AbstractCompiler,
        TestBrief,
        PID,
        CompilerResult} from "./Interface";
import {OfflineCompiler} from "./OfflineCompiler";
import {AbstractStorage,
        ProjectID,
        FileID} from "../Storage/Interface";

export {OnlineCompiler};

class OnlineCompiler extends AbstractCompiler {

  private socket: SeashellWebsocket;
  private offlineCompiler: OfflineCompiler;

  constructor(socket: SeashellWebsocket, storage: AbstractStorage, offComp: OfflineCompiler) {
    super(storage);
    this.socket = socket;
    this.offlineCompiler = offComp;
  }
  
  public async compileAndRunProject(proj: ProjectID, question: string, file: FileID, runTests: boolean): Promise<CompilerResult> {
    if(!this.socket.isConnected()) {
      return this.offlineCompiler.compileAndRunProject(proj, question, file, runTests);
    }
    const tests = await this.getTestsForQuestion(proj, question);
    return this.socket.sendMessage({
      type: 'compileAndRunProject',
      project: proj,
      question: question,
      tests: tests.map((tst: TestBrief)=>{ return tst.name; })
    }) as Promise<CompilerResult>;
  }

  public async programInput(pid: PID, contents: string): Promise<void> {
  }

  public async startIO(project: ProjectID, pid: PID): Promise<void> {
  }
}
