import {SeashellWebsocket,
        WebsocketResult} from "../Websocket/WebsocketClient";
import {Connection} from "../Services";
import {AbstractCompiler,
        TestBrief,
        CompilerResult,
        CompilerDiagnostic,
        CompilerError} from "./Interface";
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

    this.socket.register_callback("io", this.handleIO());
    this.socket.register_callback("test", this.handleTest());
  }

  public async compileAndRunProject(proj: ProjectID, question: string, file: FileID, runTests: boolean): Promise<CompilerResult> {
    if (!this.socket.isConnected()) {
      return this.offlineCompiler.compileAndRunProject(proj, question, file, runTests);
    } else if (this.activePIDs.length > 0) {
      throw new CompilerError("Cannot run a program while a program is already running.");
    }

    let tests: TestBrief[] = [];
    if (runTests) {
      tests = await this.getTestsForQuestion(proj, question);
    }

    let result: any = null;
    try {
      result = await this.socket.sendMessage({
        type: "compileAndRunProject",
        project: proj,
        question: question,
        tests: tests.map((tst: TestBrief) => { return tst.name; })
      });
    } catch (res) {
      if (!res.status || res.status !== "compile-failed") {
        throw result;
      }
      result = res;
    }

    // Handle compiler diagnostics
    result.messages = result.messages.map((msg: [boolean, string, number, number, string]): CompilerDiagnostic => {
      return {
        // For some reason msg[0] is always true in the backend response,
        //  so we will rely on whether compilation failed or not...
        error: result.status === "compile-failed",
        file: msg[1],
        line: msg[2],
        column: msg[3],
        message: msg[4]
      };
    });
    this.buffer.outputDiagnostics(result.messages);

    // Start running the program
    if (result.status === "running") {
      const pids = runTests ? result.pids : [result.pid];
      this.activePIDs = this.activePIDs.concat(pids);
      pids.map((pid: number) => {
        this.socket.sendMessage({
          type: "startIO",
          project: proj,
          pid: pid
        });
      });
    }
    return {
      messages: result.messages,
      status: result.status
    } as CompilerResult;
  }

  public async programKill(): Promise<void> {
    if (this.activePIDs.length > 0 && this.socket.isConnected()) {
      await Promise.all(this.activePIDs.map((pid: number) => {
        return this.socket.sendMessage({
          type: "programKill",
          pid: pid
        });
      }));
      // In case we have some weirdness with disconnecting & reconnecting,
      //  let's kill any programs running offline as well
      try {
        await this.offlineCompiler.programKill();
      } catch (err) {
        // Ignore the case where there are no programs running offline.
      }
    } else {
      return this.offlineCompiler.programKill();
    }
  }

  public async programInput(contents: string): Promise<void> {
    if (this.activePIDs.length === 0 || !this.socket.isConnected()) {
      return this.offlineCompiler.programInput(contents);
    } else if (this.activePIDs.length > 1) {
      throw new CompilerError("Sending input to program when multiple programs are running.");
    }
    try {
      await this.offlineCompiler.programKill();
    } catch (err) { /* Ignore if there is no program running offline */}
    return this.socket.sendMessage({
      type: "programInput",
      pid: this.activePIDs[0],
      contents: contents
    });
  }

  public async sendEOF(): Promise<void> {
    if (this.activePIDs.length === 0 || !this.socket.isConnected()) {
      return this.offlineCompiler.sendEOF();
    } else if (this.activePIDs.length > 1) {
      throw new CompilerError("Sending EOF to program when multiple programs are running.");
    }
    try {
      await this.offlineCompiler.programKill();
    } catch (err) { /* Ignore if there is no program running offline */}
    return this.socket.sendMessage({
      type: "sendEOF",
      pid: this.activePIDs[0]
    });
  }

  protected programDone(pid: number) {
    const ind = this.activePIDs.indexOf(pid);
    if (ind === -1) {
      throw new CompilerError("Program that was not running has ended.");
    } else {
      this.activePIDs.splice(ind, 1);
    }
    // if everything has finished running, notify frontend
    if (this.activePIDs.length === 0) {
      this.dispatch({
        type: "programDone",
        payload: null
      });
    }
  }
}
