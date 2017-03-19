import {ProjectID, FileID, File} from "../Storage/Interface";

export {AbstractCompiler,
        CompilerResult,
        Test,
        PID};

abstract class AbstractCompiler {
  public abstract async compileAndRunProject(proj: ProjectID, question: string, file: FileID, tests: Test[]): Promise<CompilerResult>;
  public async programKill(pid: PID): Promise<void> {
    return pid.kill();
  }
  public async sendEOF(pid: PID): Promise<void> {
    return pid.sendEOF();
  }
  public abstract async programInput(pid: PID, contents: string): Promise<void>;
  public abstract async startIO(project: ProjectID, pid: PID): Promise<void>;
}

interface Test {
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
