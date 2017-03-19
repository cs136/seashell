import {AbstractStorage,
        ProjectID,
        FileID,
        FileBrief,
        File,
        ext,
        basename} from "../Storage/Interface";
import {groupBy} from "../utils";

export {AbstractCompiler,
        CompilerResult,
        CompilerMessage,
        TestBrief,
        Test,
        PID};

abstract class AbstractCompiler {
  constructor(protected storage: AbstractStorage) { }

  // Outward-facing Compiler interface
  public abstract async compileAndRunProject(proj: ProjectID, question: string, file: FileID, runTests: boolean): Promise<CompilerResult>;
  public async programKill(pid: PID): Promise<void> {
    return pid.kill();
  }
  public async sendEOF(pid: PID): Promise<void> {
    return pid.sendEOF();
  }
  public abstract async programInput(pid: PID, contents: string): Promise<void>;
  public abstract async startIO(project: ProjectID, pid: PID): Promise<void>;
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

interface PID {
  source: AbstractCompiler;
  kill(): Promise<void>;
  sendEOF(): Promise<void>;
  startIO(): Promise<void>;
}

interface CompilerResult {
  messages: CompilerMessage[];
  pid: PID;
  status: string;
}

interface CompilerMessage {
  error: boolean;
  file: string;
  line: number;
  column: number;
  message: string;
}
