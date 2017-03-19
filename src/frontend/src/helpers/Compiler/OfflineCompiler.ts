import {AbstractCompiler,
        Test,
        CompilerResult} from "./Interface";
import {AbstractStorage,
        ProjectID,
        FileID} from "../Storage/Interface";

export {OfflineCompiler};

class OfflineCompiler extends AbstractCompiler {

  constructor(storage: AbstractStorage) {
    super(storage);
  }

  public async compileAndRunProject(proj: ProjectID, question: string, file: FileID, runTests: boolean): Promise<CompilerResult> {
    return {
      messages: [],
      pid: null,
      status: "failed"
    };
  }
  
  public async programKill(): Promise<void> { }

  public async programInput(contents: string): Promise<void> { }

  public async sendEOF(): Promise<void> { }
}
