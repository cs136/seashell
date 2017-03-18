import {AbstractCompiler, PID, Test, CompilerResult} from "./Interface";
import {ProjectID, FileID} from "../Storage";

export {OfflineCompiler};

class OfflineCompiler extends AbstractCompiler {

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
