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
import md5 = require("md5");

// polyfills
import WebSocket = require("ws");

(<any>window).WebSocket = WebSocket;
(<any>window).indexedDB = FakeIndexedDB;
(<any>window).IDBKeyRange = FDBKeyRange;

(<any>jasmine).DEFAULT_TIMEOUT_INTERVAL = 80 * 1000;

const testSize = 3;
const halfTestSize = Math.ceil(testSize / 2);

let unique = 0;
// should return a thunk, see jscheck documentation
function uniqStr(len: number): () => string {
  return () =>  J.string(J.number(0, len),
                    J.one_of([J.character("a", "z"),
                              J.character("0", "9"),
                              J.character("A", "Z"),
                              J.character("-"),
                              J.character(" "),
                              J.character("_")]))() + (++unique);
}

function uniqStrArr(arrLen: number, strLen: number): () => string[] {
  return J.array(arrLen, uniqStr(strLen));
}

Services.init(null, {
  debugWebSocket: false,
  debugWebStorage: false,
  debugService: false
});

if (TestAccount.user) {
  describe("Testing WebStorage interface", websocketTests);
  // describe.skip("Testing offline mode synchronization", syncTests);
} else {
  describe.skip("Skipped websocket related tests. You need to set up account.json", () => {
    it("skipping");
  });
}

function websocketTests() {

  let socket = Services.storage();

  beforeAll(() => {
    return Services.login(TestAccount.user, TestAccount.password);
  });

  afterAll(() => {
    Services.logout();
  });

  it("syncAll: sync multiple times. Should not crash.", async () => {
    for (const i of R.range(0, 3)) {
      await socket.syncAll();
    }
  });

  let projs: Project[] = R.sortBy(prop("id"), map((s: string) => ({
    id: `X${s}`,
    name: `X${s}`,
    runs: {},
    opened_tabs: {},
    last_modified: 0
  }), uniqStrArr(testSize, 20)()));

  let files: File[] = R.sortBy(prop("id"), map((p: Project) => {
    const name = `default/${uniqStr(20)()}.${J.one_of(["c", "h", "rkt", "txt", "test"])()}`;
    const fid = md5(p.id + name);
    const text = uniqStr(5000)();
    return {
      id: fid,
      project: p.name,
      name: name,
      contents: text,
      checksum: md5(text),
      last_modified: 0
    };
  }, flatten(repeat(projs, testSize))));


  async function remoteProjs() {
    let remoteProjs: ProjectBrief[] = await socket.getProjects();
    const remote: Project[] = [];
    for (const p of remoteProjs) {
      remote.push(await socket.getProject(p.id));
    }
    return R.sortBy(prop("id"), map((p) => ({
      // properties you want to check
      id: p.id,
      name: p.name,
      runs: p.runs,
      last_modified: 0,
      opened_tabs: p.opened_tabs
    }), remote)) || [];
  }


  async function remoteFiles() {
    let remoteFiles = await socket.getAllFiles();
    for (let x of remoteFiles) {
      const file = await socket.readFile(x.id);
      Object.assign(x, file);
    }
    remoteFiles = R.sortBy(prop("id"), remoteFiles) || [];
    return map((x) => ({
      id: x.id,
      project: x.project,
      name: x.name,
      contents: x.contents,
      checksum: x.checksum,
      last_modified: 0
    }), <File[]> remoteFiles);
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

  it("getFileToRun: should return false when no run files", async () => {
    for (const p of projs) {
      const f = await socket.getFileToRun(p.name, "default");
      expect(f).toEqual(false);
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
      const r = await socket.readFile(f.id);
      const a = [f.name, f.project, f.contents];
      const b = [r.name, r.project, r.contents];
      expect(a).toEqual(b);
    };
  });

  it(`writeFile: update ${halfTestSize} files`, async () => {
    const fs = R.take(halfTestSize, files);
    for (const f of fs) {
      f.contents = uniqStr(5000)();
      f.checksum = md5(f.contents);
      await socket.writeFile(f.id, f.contents);
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
  });

  function genRunFiles(p: string): {[index: string]: FileID} {
    return R.zipObj(uniqStrArr(testSize, 30)(),
                    J.array(testSize, J.one_of(map((file) => [p, file.name], files)))());
  }

  it(`setFileToRun: randomly pick a run file per project`, async () => {
    for (const f of files) {
      await socket.setFileToRun(f.project, "default", f.id);
      const pj = R.find((x) => x.id === f.project, projs);
      pj.runs.default = f.id;
      for (const o of files) {
        const check = await socket.getFileToRun(o.project, "default");
        if (o.project ===  f.project) {
          expect(check).toEqual(f.id);
        } else {
          expect(check).not.toEqual(f.id);
        }
      }
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });

  it(`renameFile: rename ${halfTestSize} files per project. Should not change the file's id.`, async () => {
    const projGps = R.groupBy((x: File) => x.project, files);
    for (const p in projGps) {
      const pj = R.find((x) => x.name === p, projs);
      console.assert(pj);
      for (const i of R.range(0, Math.min(halfTestSize, projGps[p].length))) {
        const file = projGps[p][i];
        const newname = file.name + "_new_name";
        await socket.renameFile(file.id, newname);
        file.name += "_new_name";
      }
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });

  it("setSettings: set random properties", async () => {
    const s: Settings = defaultSettings;
    s.font_size = 100;
    await socket.setSettings(s);
    const r = await socket.getSettings();
    expect(r).toEqual(s);
  });

  it(`deleteFile: delete ${halfTestSize} files per project. Should also remove from run files`, async () => {
    const projGps = R.groupBy((x: File) => x.project, files);
    console.assert(projGps);
    let removed: File[] = [];
    for (const p in projGps) {
      const pj: Project = R.find((x) => x.id === p, projs);
      console.assert(pj);
      for (const i of R.range(0, Math.min(halfTestSize, projGps[p].length))) {
        const file = projGps[p][i];
        await socket.setFileToRun(pj.name, "default", file.id);
        await socket.deleteFile(file.id);
        removed.concat(filter((x) => x.id === file.id, files));
        files = filter((x) => x.id !== file.id, files);
        for (const d in pj.runs) {
          if (pj.runs[d] === file.id) {
            delete pj.runs[d];
          }
        }
        delete pj.runs.default;
      }
    }
    const remote: File[] = await remoteFiles();
    expect(R.intersectionWith(R.eqProps("id"), remote, removed)).toEqual([]);
    expect(remote).toEqual(expect.arrayContaining(files));
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });

  it(`deleteProject: delete ${halfTestSize} projects. Should also delete children.`, async () => {
    for (const i of R.range(0, halfTestSize)) {
      const proj = projs[i];
      await socket.deleteProject(proj.id);
      projs = R.remove(i, 1, projs);
      files = R.filter((f) => f.project !== proj.id, files);
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
    // expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });
}


function syncTests() {

  let socket = Services.storage();

  beforeAll(() => {
    return Services.login(TestAccount.user, TestAccount.password);
  });

  afterAll(() => {
    Services.logout();
  });

  it.skip("Initial state check", async () => {
  });

  it.skip("writeFile should leave a ChangeLog", async () => {

  });

  it.skip("Write to the same file mutiple times should leave a single ChangeLog", async () => {

  });


}
