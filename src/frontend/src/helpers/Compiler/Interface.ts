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

  protected abstract programDone(pid: number): void;

  private stdout: string;
  private stderr: string;
  private timeout: any;

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

  private output(out: string): void {
    this.dispatch({
      type: "output",
      payload: out
    });
  }

  private flush(): void {
    this.output(this.stdout + this.stderr);
    this.stdout = "";
    this.stderr = "";
  }

  private outputASAN(ASAN: ASANOutput): string {
    if (ASAN.error_type === "unknown" && ASAN.raw_message === "")
      return "";
    let output = "";
    output += "Memory error occurred! Type of error: " + ASAN.error_type + "\n";
    if (ASAN.call_stacks.length === 0) {
      // If no call stack was sent back, fall back to showing the raw ASAN output
      output += "Raw error message:\n";
      output += ASAN.raw_message;
    }
    for (let stack = 0; stack < ASAN.call_stacks.length; stack++) {
      const framelist = ASAN.call_stacks[stack].framelist;
      const fmisc = ASAN.call_stacks[stack].misc;
      const indent = framelist.length <= 1 ? "\t" : "\t  ";
      for (let frame = 0; frame < framelist.length; frame++) {
        output += indent + "frame " + framelist[frame].frame + ":" +
          " function " + framelist[frame].function +
          " in line " + framelist[frame].line +
          ("column" in framelist[frame] ? ", column " + framelist[frame].column : "") + "\n";
      }
      for (let key in fmisc) {
        output += "\t" + key.replace(/_/g, " ") + ": " + fmisc[key] + "\n";
      }
    }
    for (let key in ASAN.misc) {
      output += key.replace(/_/g, " ") + ": " + ASAN.misc[key] + "\n";
    }
    return output;
  }

  protected handleIO(result: IOMessage): void {
    let output = "";
    if (result.type === "stdout") {
      this.stdout += result.message;
      const spl = this.stdout.split("\n");
      for (let i = 0; i < spl.length - 1; i++) {
        output += spl[i] + "\n";
      }
      this.stdout = spl[spl.length - 1];
    } else if (result.type === "stderr") {
      this.stderr += result.message;
      const spl = this.stderr.split("\n");
      for (let i = 0; i < spl.length - 1; i++) {
        output += spl[i] + "\n";
      }
      this.stderr = spl[spl.length - 1];
    } else if (result.type === "done") {
      this.flush();
      const ASAN = result.asan_output ? JSON.parse(result.asan_output) : false;
      if (ASAN) {
        output += this.outputASAN(ASAN);
      }
      output += "Program finished with exit code " + result.status + ".\n";
      this.programDone(result.pid);
    }
    this.output(output);
    clearTimeout(this.timeout);
    this.timeout = setTimeout(this.flush, 100);
  }

  protected handleTest(result: TestMessage): void {
    let output = "----------------------------------\n";
    const ASAN = result.asan_output ? JSON.parse(result.asan_output) : false;
    if (result.result === "passed") {
      output += "Test \"" + result.test_name + "\" passed.\n";
    } else if (result.result === "failed") {
      output += "Test \"" + result.test_name + "\" failed.\n";
      output += "Produced output (stdout):\n";
      output += result.stdout;
      output += "Expected output (stdout):\n";
      const diffStr = (ln: DiffLine): string => {
        if (typeof ln === "string") {
          return ln;
        } else {
          return ln[1];
        }
      };
      if (result.diff.length > 0) {
        output += diffStr(result.diff[0]);
      }
      for (let i = 1; i < result.diff.length; i++) {
        output += "\n" + diffStr(result.diff[i]);
      }
    } else if (result.result === "error") {
      output += "Test \"${result.test_name}\" caused an error (with return code ${result.status})!\n";
      output += "Produced output (stdout):\n";
      output += result.stdout;
    } else if (result.result === "no-expect") {
      output += "Test \"${result.test_name}\" produced output (stdout):\n";
      output += result.stdout;
    } else if (result.result === "timeout") {
      output += "Test \"${result.test_name}\" timed out.\n";
    } else if (result.result === "killed") {
      output += "Test \"${result.test_name}\" was killed.\n";
    };
    if (result.stderr !== "") {
      output += "Produced errors (stderr):\n";
      output += result.stderr;
    }
    if (ASAN && ASAN.raw_message !== "") {
      output += "AddressSanitizer Output:\n";
      output += this.outputASAN(ASAN);
    }
    this.programDone(result.pid);
    this.output(output);
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
