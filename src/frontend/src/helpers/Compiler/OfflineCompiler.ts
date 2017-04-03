import {AbstractCompiler,
        Test,
        CompilerResult,
        IOMessage} from "./Interface";
import {AbstractStorage,
        ProjectID,
        FileID,
        File} from "../Storage/Interface";
import {DispatchFunction} from "../Services";
import {CompilerError} from "../Errors";

export {OfflineCompiler};

const CompilerWorker = (() => {
  if (IS_BROWSER) {
    return require("worker-loader!../../workers/offline-compile.js");
  } else {
    // TODO: Polyfill WebWorkers for Jest/Node.js
    return null;
  }
})();

const RunnerWorker = (() => {
  if (IS_BROWSER) {
    return require("worker-loader!../../workers/offline-run.js");
  } else {
    // TODO: Polyfill WebWorkers for Jest/Node.js
    return null;
  }
})();

interface PID {
  id: number;
  runner: Worker;
}

interface CompilerWorkerResult {
  data: CompilerResult;
  type: string;
}

interface RunnerWorkerResult {
  data: IOMessage;
}

class OfflineCompiler extends AbstractCompiler {

  constructor(storage: AbstractStorage, dispatch: DispatchFunction) {
    super(storage, dispatch);

    this.freePID = 0;
    this.activePIDs = [];
  }

  private freePID: number;
  private activePIDs: PID[];

  // For now we will just grab all files for the question as the dependencies.
  private async getDependencies(proj: ProjectID, question: string): Promise<File[]> {
    // TODO
    return [];
  }

  public async compileAndRunProject(proj: ProjectID, question: string, file: FileID, runTests: boolean): Promise<CompilerResult> {
    return new Promise<CompilerResult>(async (resolve, reject) => {
      let compiler = new CompilerWorker();
      compiler.onmessage = (result: CompilerWorkerResult) => {
        if (result.data.status === "compile-failed") {
          resolve(result.data);
        } else if (result.data.status === "running") {
          const pid = ++this.freePID;
          let runner = new RunnerWorker();
          runner.onmessage = (result: RunnerWorkerResult) => {
            this.handleIO(result.data);
          };
          this.activePIDs.push({
            id: pid,
            runner: runner
          });
        }
      };
      compiler.postMessage({
        project: proj,
        question: question,
        runnerFile: file,
        files: await this.getDependencies(proj, question)
      });
    });
  }

  public async programKill(): Promise<void> {
    for (let i = 0; i < this.activePIDs.length; i++) {
      this.activePIDs[i].runner.postMessage({
        type: "kill"
      });
    }
  }

  public async programInput(contents: string): Promise<void> {
    if (this.activePIDs.length !== 1) {
      throw new CompilerError("Can only send input when exactly one program is running.");
    }
    this.activePIDs[0].runner.postMessage(contents);
  }

  public async sendEOF(): Promise<void> {
    if (this.activePIDs.length !== 1) {
      throw new CompilerError("Can only send EOF when exactly one program is running.");
    }
    this.activePIDs[0].runner.postMessage(null);
  }

  protected programDone(pid: number): void {
    const found = this.activePIDs.filter((item: PID) => {
      return item.id === pid;
    });
    if (found.length !== 1) {
      throw new CompilerError("Program that is not running has stopped running.");
    }
    this.activePIDs = this.activePIDs.filter((item: PID) => {
      return item.id !== pid;
    });
  }
}
