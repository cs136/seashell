import {AbstractCompiler,
        TestBrief,
        Test,
        CompilerResult,
        IOMessage,
        TestMessage} from "./Interface";
import {AbstractStorage,
        ProjectID,
        FileID,
        File} from "../Storage/Interface";
import {DispatchFunction} from "../Services";
import {CompilerError} from "../Errors";
import {appStateActions} from "../../reducers/appStateReducer";
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

interface TesterWorkerResult {
  data: TestMessage;
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
    return await this.storage.getFiles(proj, question, true);
  }

  private initTest(test: Test): PID {
    const pid = ++this.freePID;
    let tester = new RunnerWorker();

    tester.onmessage = (result: TesterWorkerResult) => {
      this.handleTest()(result.data);
    };

    tester.postMessage({
      type: "testdata",
      pid: pid,
      test_name: test.name,
      in: test.in ? test.in.contents : undefined,
      expect: test.expect ? test.expect.contents : undefined
    });

    return {
      id: pid,
      runner: tester
    };
  }

  public async compileAndRunProject(proj: ProjectID, question: string, file: FileID, runTests: boolean): Promise<CompilerResult> {
    return new Promise<CompilerResult>(async (resolve, reject) => {
      let compiler = new CompilerWorker();
      compiler.onmessage = async (result: CompilerWorkerResult) => {
        if (result.data.status === "error") {
          throw new CompilerError(result.data.err || "Unknown error");
        } else {
          this.buffer.outputDiagnostics(result.data.messages);
          resolve(result.data);
        }
        if (result.data.status === "running") {
          if (!runTests) {
            // run the program interactively
            const pid = ++this.freePID;
            let runner = new RunnerWorker();
            runner.onmessage = (result: RunnerWorkerResult) => {
              this.handleIO()(result.data);
            };
            this.activePIDs.push({
              id: pid,
              runner: runner
            });
            runner.postMessage(result.data.obj);
          } else {
            // run all the tests
            let tests: Test[] = await this.getTestsForQuestion(proj, question);
            if (tests.length < 2) {
              throw new CompilerError("There are no tests to run for this question.");
            }
            for (let i = 0; i < tests.length; i++) {
              const tester = this.initTest(tests[i]);
              this.activePIDs.push(tester);
              tester.runner.postMessage(result.data.obj);
            }
          }
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

  public programDone(pid: number): void {
    const found = this.activePIDs.filter((item: PID) => {
      return item.id === pid;
    });
    if (found.length !== 1) {
      console.warn("Program that is not running (online) has stopped running.");
    }
    this.activePIDs = this.activePIDs.filter((item: PID) => {
      return item.id !== pid;
    });
    if (this.activePIDs.length === 0) {
      this.dispatch({
        type: appStateActions.setNotRunning,
        payload: null
      });
    }
  }
}
