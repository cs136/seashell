import {AbstractStorage,
        ProjectID,
        FileID,
        FileBrief,
        File,
        ext,
        basename} from "../Storage/Interface";
import {groupBy} from "../utils";
import {DispatchFunction} from "../Services";
import {CompilerError} from "../Errors";
import {OutputBuffer} from "./OutputBuffer";

export {AbstractCompiler,
        CompilerResult,
        CompilerMessage,
        TestBrief,
        Test,
        CompilerError,
        IOMessage,
        TestMessage,
        DiffLine,
        ASANOutput};

abstract class AbstractCompiler {

  constructor(protected storage: AbstractStorage, protected dispatch: DispatchFunction) {
    this.buffer = new OutputBuffer(dispatch);
  }

  // Outward-facing Compiler interface
  public abstract async compileAndRunProject(proj: ProjectID, question: string, file: FileID, runTests: boolean): Promise<CompilerResult>;
  public abstract async programKill(): Promise<void>;
  public abstract async sendEOF(): Promise<void>;
  public abstract async programInput(contents: string): Promise<void>;
  // End public interface

  protected abstract programDone(pid: number): void;

  private buffer: OutputBuffer;

  // Function used by both compilers to group the test files appropriately
  //  to send to their respective backends
  protected async getTestsForQuestion(project: ProjectID, question: string): Promise<TestBrief[]> {
    const files = await this.storage.getFiles(project);
    return groupBy(files.filter((f: FileBrief) => {
      return f.name.startsWith(question + "/tests/");
    }), (f: FileBrief) => {
      return basename(f);
    }).map((t: FileBrief[]) => {
      let test: TestBrief = {
        name: basename(t[0]),
        in: null,
        expect: null
       };
      if (ext(t[0]) === "in")
        test.in = t[0];
      else
        test.expect = t[0];
      if (t[1] && ext(t[1]) === "in")
        test.in = t[1];
      else if (t[1])
        test.expect = t[1];
      return test;
    });
  }

  protected handleIO(result: IOMessage): void {
    this.buffer.outputIO(result);
    if (result.type === "done") {
      this.programDone(result.pid);
    }
  }

  protected handleTest(result: TestMessage): void {
    this.buffer.outputTest(result);
    this.programDone(result.pid);
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
  obj?: string;
  err?: string;
}

interface CompilerMessage {
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
  asan_output?: string;
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
