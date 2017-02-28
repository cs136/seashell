// Temporary file holding some type definitions that need to be fleshed out
//  in other parts of the code

export class Settings { }

export class History { }

export class Test {
  test_name: string;
}

export class Change { }

export class SeashellFile {
  public ext(): string { return ""; }
  public toWorker(): Object { return {}; }
  public async getDependencies(question: string): Promise<Array<SeashellFile>> { return []; }
  public fullname(): string { return ""; }
}

export class Storage {
  [key: string]: Function;
  public isInitialized() { }
  public async getProjectsForSync(): Promise<Array<string>> { return []; }
  public async listAllProjectsForSync(): Promise<Array<string>> { return []; }
  public async getOfflineChanges(): Promise<Array<Change>> { return []; }
  public applyChanges(change: Array<Change>, newProjects: Array<string>, deletedProjects: Array<string>, updatedProjects: Array<string>, settings: Settings) { }
  public hasOfflineChanges(): boolean { return false; }
  public getSettings(opt?: boolean): any { return {}; }
}

export class SeashellCompiler {
  public async compile(project: string, question: string, files: Array<Object>, runner: string): Promise<any> { return {}; }
}

export class SeashellRunner {
  public async run(obj: Uint8Array, cb: (msg: any, data: any)=>any): Promise<any> { return {}; }
}

export class SeashellTester {
  public runTests(obj: Uint8Array, cb: (msg: any, data: any)=>any, tests: Array<Test>) { }
}

export class SeashellPID {
  public kill() { }
  public sendEOF() { }
  public startIO() { }
}
