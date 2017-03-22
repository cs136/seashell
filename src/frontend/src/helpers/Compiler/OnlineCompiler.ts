import {SeashellWebsocket,
        WebsocketResult} from "../Websocket/WebsocketClient";
import {Connection} from "../Services";
import {AbstractCompiler,
        TestBrief,
        CompilerResult} from "./Interface";
import {OfflineCompiler} from "./OfflineCompiler";
import {AbstractStorage,
        ProjectID,
        FileID} from "../Storage/Interface";
import {DispatchFunction} from "../Services";

export {OnlineCompiler};

class OnlineCompiler extends AbstractCompiler {

  private socket: SeashellWebsocket;
  private offlineCompiler: OfflineCompiler;
  private activePIDs: number[];

  constructor(socket: SeashellWebsocket, storage: AbstractStorage, offComp: OfflineCompiler, dispatch: DispatchFunction) {
    super(storage, dispatch);
    this.socket = socket;
    this.offlineCompiler = offComp;
    this.activePIDs = [];
  }
  
  public async compileAndRunProject(proj: ProjectID, question: string, file: FileID, runTests: boolean): Promise<CompilerResult> {
    if(!this.socket.isConnected()) {
      return this.offlineCompiler.compileAndRunProject(proj, question, file, runTests);
    }
    let tests: TestBrief[] = [];
    if(runTests) {
      tests = await this.getTestsForQuestion(proj, question);
    }
    const result = await this.socket.sendMessage({
      type: 'compileAndRunProject',
      project: proj,
      question: question,
      tests: tests.map((tst: TestBrief)=>{ return tst.name; })
    });
    if(result.status == "running") {
      this.activePIDs.push(result.pid);
    }
    return {
      messages: result.messages,
      status: result.status
    };
  }

  public async programKill(): Promise<void> { }

  public async programInput(contents: string): Promise<void> { }

  public async sendEOF(): Promise<void> { }
}
