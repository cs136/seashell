import {CompilerDiagnostic,
        IOMessage,
        TestMessage,
        DiffLine,
        ASANOutput} from "./Interface";
import {DispatchFunction} from "../Services";
import {groupBy} from "../utils";

export {OutputBuffer};

class OutputBuffer {

  private stdout: string;
  private stderr: string;
  private timeout: any;

  constructor(private dispatch: DispatchFunction) {
    this.stdout = "";
    this.stderr = "";
  }

  private output(out: string): void {
    this.dispatch({
      type: "console_write",
      payload: {content: out}
    });
  }

  private flush(): () => void {
    return (): void => {
      this.output(this.stdout + this.stderr);
      this.stdout = "";
      this.stderr = "";
    }
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

  public outputIO(result: IOMessage): void {
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
      this.flush()();
      const ASAN = result.asan_output ? JSON.parse(result.asan_output) : false;
      if (ASAN) {
        output += this.outputASAN(ASAN);
      }
      output += `Program finished with exit code ${result.status}.\n`;
    }
    this.output(output);
    clearTimeout(this.timeout);
    this.timeout = setTimeout(this.flush(), 100);
  }

  public outputTest(result: TestMessage): void {
    let output = "----------------------------------\n";
    const ASAN = result.asan_output ? JSON.parse(result.asan_output) : false;
    if (result.result === "passed") {
      output += `Test "${result.test_name}" passed.\n`;
    } else if (result.result === "failed") {
      output += `Test "${result.test_name}" failed.\n`;
    } else if (result.result === "error") {
      output += `Test "${result.test_name}" caused an error!\n`;
    } else if (result.result === "no-expect") {
      output += `Test "${result.test_name}" completed.\n`;
    } else if (result.result === "timeout") {
      output += `Test "${result.test_name}" timed out.\n`;
    } else if (result.result === "killed") {
      output += `Test "${result.test_name}" was killed.\n`;
    };
    if (result.result !== "passed") {
      output += "Produced output (stdout):\n";
      output += result.stdout;
    }
    if (result.result === "failed") {
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
    }
    if (result.stderr !== "") {
      output += "Produced errors (stderr):\n";
      output += result.stderr;
    }
    if (ASAN && ASAN.raw_message !== "") {
      output += "AddressSanitizer Output:\n";
      output += this.outputASAN(ASAN);
    }
    this.output(output);
  }

  public outputDiagnostics(diags: CompilerDiagnostic[]): void {
    if (!diags.length) {
      return;
    }
    const warnOnly = !groupBy(diags, (d: CompilerDiagnostic) => {
      return d.error ? "err" : "warn";
    }).err;
    let output = "";
    if (warnOnly) {
      output += "Compilation generated warnings:\n";
    } else {
      output += "Compilation failed with errors:\n";
    }
    // Remove excessive undefined main linker errors
    diags = diags.filter((d: CompilerDiagnostic) => {
      return !(d.message.endsWith("In function `_start':") ||
        /relocation \d+ has invalid symbol index \d+$/.test(d.message));
    });
    for (let i = 0; i < diags.length; i++) {
      output += `${diags[i].file}:${diags[i].line}:${diags[i].column}: ${diags[i].message}\n`;
    }
    this.output(output);
  }

}
