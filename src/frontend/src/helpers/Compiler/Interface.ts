import {AbstractStorage,
        ProjectID,
        FileID,
        FileBrief,
        File,
        ext,
        basename} from "../Storage/Interface";
import {groupBy} from "../utils";
import {DispatchFunction} from "../Services";

export {AbstractCompiler,
        CompilerResult,
        CompilerMessage,
        TestBrief,
        Test,
        CompilerError};

abstract class AbstractCompiler {
  constructor(protected storage: AbstractStorage, protected dispatch: DispatchFunction) { }

  // Outward-facing Compiler interface
  public abstract async compileAndRunProject(proj: ProjectID, question: string, file: FileID, runTests: boolean): Promise<CompilerResult>;
  public abstract async programKill(): Promise<void>;
  public abstract async sendEOF(): Promise<void>;
  public abstract async programInput(contents: string): Promise<void>;
  // End public interface

  // Function used by both compilers to group the test files appropriately
  //  to send to their respective backends
  protected async getTestsForQuestion(project: ProjectID, question: string): Promise<TestBrief[]> {
    const files = await this.storage.getFiles(project);
    return groupBy(files.filter((f: FileBrief)=> {
      return f.name.startsWith(question+"/tests/");
    }), (f: FileBrief)=>{
      return basename(f);
    }).map((t: FileBrief[])=> {
      let test: TestBrief = {
        name: basename(t[0]),
        in: null,
        expect: null
       };
      if(ext(t[0]) == "in")
        test.in = t[0];
      else
        test.expect = t[0];
      if(t[1] && ext(t[1]) == "in")
        test.in = t[1];
      else if(t[1])
        test.expect = t[1];
      return test;
    });
  }
}

interface TestBrief {
  name: string;
  in: FileBrief;
  expect: FileBrief;
}

interface Test {
  name: string;
  in: File;
  expect: File;
}

interface CompilerResult {
  messages: CompilerMessage[];
  status: string;
}

interface CompilerMessage {
  error: boolean;
  file: string;
  line: number;
  column: number;
  message: string;
}

class CompilerError extends Error {
  data: Object;
  constructor(msg: string, data?: Object) {
    super(msg);
    this.data = data;
  }
}
