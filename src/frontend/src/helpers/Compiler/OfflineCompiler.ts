import {AbstractCompiler,
        Test,
        CompilerResult} from "./Interface";
import {AbstractStorage,
        ProjectID,
        FileID} from "../Storage/Interface";
import {DispatchFunction} from "../Services";

export {OfflineCompiler};

class OfflineCompiler extends AbstractCompiler {

  constructor(storage: AbstractStorage, dispatch: DispatchFunction) {
    super(storage, dispatch);
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
