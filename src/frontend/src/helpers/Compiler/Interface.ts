import {ProjectID,
        FileID,
        File} from "../Storage/Interface";
import {LocalStorage} from "../Storage/LocalStorage";
import {groupBy} from "../utils";
import {DispatchFunction} from "../Services";
import {CompilerError} from "../Errors";
import {OutputBuffer} from "./OutputBuffer";

export {AbstractCompiler,
        CompilerResult,
        CompilerDiagnostic,
        TestBrief,
        Test,
        CompilerError,
        IOMessage,
        TestMessage,
        DiffLine,
        ASANOutput};

abstract class AbstractCompiler {

  constructor(protected storage: LocalStorage, protected dispatch: DispatchFunction) {
    this.buffer = new OutputBuffer(dispatch);
  }

  // Outward-facing Compiler interface
  public abstract async compileAndRunProject(proj: ProjectID, question: string, file: FileID, runTests: boolean): Promise<CompilerResult>;
  public abstract async programKill(): Promise<void>;
  public abstract async sendEOF(): Promise<void>;
  public abstract async programInput(contents: string): Promise<void>;
  public abstract programDone(pid: number): void;
  // End public interface

  protected buffer: OutputBuffer;

  // Function used by both compilers to group the test files appropriately
  //  to send to their respective backends
  protected async getTestsForQuestion(project: ProjectID, question: string): Promise<TestBrief[]> {
    const files = await this.storage.getFiles(project, question, true);
    return groupBy(files.filter((f: File) => {
      return f.name.startsWith(question + "/tests/");
    }), (f: File) => {
      return f.basename();
    }).map((t: File[]) => {
      let test: TestBrief = {
        name: t[0].basename(),
        in: undefined,
        expect: undefined
       };
      if (t[0].extension() === "in")
        test.in = t[0];
      else
        test.expect = t[0];
      if (t[1] && t[1].extension() === "in")
        test.in = t[1];
      else if (t[1])
        test.expect = t[1];
      return test;
    });
  }

  protected handleIO(): (result: IOMessage) => void {
    return (result: IOMessage): void => {
      this.buffer.outputIO(result);
      if (result.type === "done") {
        this.programDone(result.pid);
      }
    };
  }

  protected handleTest(): (result: TestMessage) => void {
    return (result: TestMessage): void => {
      this.buffer.outputTest(result);
      this.programDone(result.pid);
    };
  }
}

interface TestBrief extends Test  {
  name: string;
  in?: File;
  expect?: File;
}

interface Test {
  name: string;
  in?: File;
  expect?: File;
}


interface CompilerResult {
  messages: CompilerDiagnostic[];
  status: string;
  obj?: string;
  err?: string;
}

interface CompilerDiagnostic {
  error: boolean;
  file: string;
  line: number;
  column: number;
  message: string;
}

interface IOMessage {
  message: string;
  pid: number;
  type: string;
  status?: number;
  asan?: string;
}

type DiffLine = string | [boolean, string];

interface TestMessage {
  pid: number;
  result: string;
  stderr: string;
  stdout: string;
  test_name: string;
  diff?: DiffLine[];
  asan_output?: string;
}

interface ASANOutput {
  call_stacks: ASANCallStack[];
  misc: {[type: string]: string};
  raw_message: string;
  error_type: string;
}

interface ASANCallStack {
  framelist: ASANStackFrame[];
  misc: {[type: string]: string};
}

interface ASANStackFrame {
  file: string;
  function: string;
  line: number;
  column: number;
  offset: string;
  frame: number;
}
