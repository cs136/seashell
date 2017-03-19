import "jest";
import {WebStorage,
        SeashellWebsocket} from "../../src/helpers/Storage/WebStorage";
import {File, FileID, FileBrief,
        Project, ProjectID, ProjectBrief,
        Settings, defaultSettings, Services} from "../../src/helpers/Services";
import {LocalStorage} from "../../src/helpers/Storage/LocalStorage";
import * as R from "ramda";
import FakeIndexedDB = require("fake-indexeddb");
import FDBKeyRange = require("fake-indexeddb/lib/FDBKeyRange");
import {map, filter, flatten, repeat, head, prop} from "ramda";
import J = require("jscheck");
import {TestAccount} from "../account";

jasmine.DEFAULT_TIMEOUT_INTERVAL = 30*1000;

const testSize = 1;
const halfTestSize = Math.ceil(testSize/2);

let unique = 0;
// should return a thunk, see jscheck documentation
function uniqStr(len: number): () => string {
  return () =>  J.string(J.number(0, len),
                    J.one_of([J.character('a','z'),
                              J.character('0','9'),
                              J.character('A','Z'),
                              J.character("-"),
                              J.character(" "),
                              J.character("_")]))() + (++unique);
}

function uniqStrArr(arrLen: number, strLen: number): () => string[] {
  return J.array(arrLen, uniqStr(strLen));
}

describe("testing websocket.ts", () => {

  let socket = Services.storage();

  beforeAll(() => {
    return Services.login(TestAccount.user, TestAccount.password).catch((err) => {
      console.error(err);
    });
  });

  afterAll(() => {
    socket.disconnect();
  });

  it("setting up tests", () => {
    expect(socket.isConnected()).toEqual(true);
    expect(socket.isOffline()).toEqual(false);
  });

  let projs = R.sortBy(prop("name"), map((s: string) => ({
    // properties you want to check
    id: `X${s}`,
    name: `X${s}`,
    // runs: {}
  }), uniqStrArr(testSize,20)()));

  let files = map((p) => {
    const name = `default/${uniqStr(20)()}.${J.one_of(["c","h","rkt","txt","test"])()}`;
    return {
      id: [p.id, name],
      project: p.id,
      name: name,
      contents: uniqStr(5000)()+"\n"
    }
  }, flatten(repeat(projs, testSize)));

  async function remoteProjs() {
    const remote = map((p) => ({
      // properties you want to check
      id: p.id,
      name: p.name,
      runs: undefined
    }), await socket.getProjects());
    // for (let p of remote) {
      // const r = await socket.getProject(p.id);
      // p.runs = r.runs;
    // }
    return remote;
  }

  async function remoteFiles() {
    let projGps = R.groupBy((x:File) => x.project, files);
    for (const p of projs) {
      let remoteFiles: File[] = <File[]> await socket.getFiles(p.id);
      // ignore automatically created file by the backend
      remoteFiles = remoteFiles.filter((f:File) => f.name != "default/main.c");
      for (let x of remoteFiles) {
        const file = await socket.readFile([p.name, x.name]);
        x.contents = file.contents;
      }
      let projFiles = projGps[p.name];
      // remoteFiles   = R.sortBy(prop("name"), remoteFiles);
      // projFiles = R.sortBy(prop("name"), projGps[p.name]);
      return map((x) => ({
        id: x.id,
        project: x.project,
        name: x.name,
        contents: x.contents,
      }), remoteFiles);
    }
  }

  it(`newProject: create ${testSize} projects`, async () => {
    for (const p of projs) {
      await socket.newProject(p.name);
    }
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });

  it(`getProjects: list all projects`, async () => {
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });

  it("getFileToRun: should return undefined when no run files", async () => {
    for (const p of projs) {
      const f = await socket.getFileToRun(p.name, "default");
      expect(f).toEqual(undefined);
    }
  });

  it(`newFile: create ${testSize} files per project`, async () => {
    for (const f of files) {
      await socket.newFile(f.project, f.name, f.contents);
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });

  it(`getFiles: list all files`, async () => {
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
  });

  it(`readFile: read all files`, async () => {
    for (const f of files) {
      const r = await socket.readFile([f.project, f.name]);
      const a = [f.name, f.project, f.contents];
      const b = [r.name, r.project, r.contents];
      expect(a).toEqual(b);
    };
  });

  it(`writeFile: update ${halfTestSize} files`, async () => {
    const fs = R.take(halfTestSize, files);
    for (const f of fs) {
      f.contents = uniqStr(5000)();
      await socket.writeFile([f.project, f.name], f.contents);
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
  });

  function genRunFiles(p: string) : {[index: string]: FileID} {
    return R.zipObj(uniqStrArr(testSize,30)(),
                    J.array(testSize, J.one_of(map((file) => [p, file.name], files)))())
  }

  it(`setFileToRun: randomly pick a run file per project`, async () => {
    for (const f of files) {
      await socket.setFileToRun(f.project, "default", [f.project, f.name]);
      for (const o of files) {
        const check = await socket.getFileToRun(o.project, "default");
        if (o.project == f.project) {
          expect(check).toEqual(f.id);
        } else {
          expect(check).not.toEqual(f.id);
        }
      }
    }
  });

  it(`renameFile: rename ${halfTestSize} file per project, should also rename run files`, async () => {
    // group files by project
    const projGps = R.groupBy((x:File) => x.project, files);
    for (const p in projGps) {
      const pj: Project = R.find((x) => x.name == p, projs);
      for (const i of R.range(0,Math.min(halfTestSize, projGps[p].length))) {
        const old: string  = projGps[p][i].name; // old name
        const name: string = (projGps[p][i].name += "_new_name");
        projGps[p][i].id = [p, projGps[p][i].name];
        await socket.setFileToRun(p, "default", [p, old]);
        await socket.renameFile([p, old], name);
        /* backend does not support
        // update local varible to reflect the rename
        // now the question's runner file should be the new name.
        pj.runs["default"] = name;
        for (const d in pj.runs) {
          if (pj.runs[d] == old) {
            pj.runs[d] = name;
          }
          const check: FileID = await socket.getFileToRun(p, d);
          expect(check).toEqual(pj.runs[d]);
        }
        //*/
      }
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });

  it("setSettings: set random properties", async () => {
    const s: Settings = defaultSettings;
    s.fontSize = 100;
    await socket.setSettings(s);
    const r = await socket.getSettings();
    expect(r).toEqual(s);
  });

  it(`deleteFile: delete ${halfTestSize} files per project, should also remove from run files`, async () => {
    const projGps = R.groupBy((x:File) => x.project, files);
    for (const p in projGps) {
      const pj = R.find((x) => x.name == p, projs);
      for (const i of R.range(0,Math.min(halfTestSize, projGps[p].length))) {
        const nm = projGps[p][i].name;
        const fid: [string, string] = [p, nm];
        await socket.setFileToRun(pj.name, nm, fid);
        await socket.deleteFile(fid);
        expect(await remoteFiles()).not.toEqual(expect.arrayContaining(files));
        expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
        files = filter((x) => x.project != p || x.name != nm, files);
        for (const d in pj.runs) {
          if (pj.runs[d] == fid) {
            delete pj.runs[d];
          }
        }
      }
    }
  });

  it(`deleteProject: delete ${halfTestSize} projects, should also delete children`, async () => {
    for (const i of R.range(0,halfTestSize)) {
      const p = projs[0].name;
      await socket.deleteProject(p);
      projs = R.tail(projs);
      files = R.filter((f) => f.project != p, files);
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });


});

