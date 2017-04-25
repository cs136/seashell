import "jest";
import {WebStorage, OfflineMode} from "../../src/helpers/Storage/WebStorage";
import {File, FileID, FileBrief, FileStored,
        Project, ProjectID, ProjectBrief, ProjectStored,
        Settings, Services} from "../../src/helpers/Services";
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
import * as LS from "localstorage-memory";
(<any>window).localStorage = LS;
(<any>window).WebSocket = WebSocket;
(<any>window).indexedDB = FakeIndexedDB;
(<any>window).IDBKeyRange = FDBKeyRange;

// test settings
(<any>jasmine).DEFAULT_TIMEOUT_INTERVAL = 80 * 1000;
const testSize = 3;
const halfTestSize = Math.ceil(testSize / 2);

// helpers
let unique = 0;

// should return a thunk, see jscheck documentation
function uniqStr(len: number): () => string {
  return () =>  J.string(J.number(0, len),
                    J.one_of([J.character("a", "z"),
                              J.character("0", "9"),
                              J.character("A", "Z"),
                              J.character("-"),
                              J.character("_")]))() + (++unique);
}

function uniqStrArr(arrLen: number, strLen: number): () => string[] {
  return J.array(arrLen, uniqStr(strLen));
}

if (TestAccount.user) {
  // describe("Testing WebStorage in offline mode", () => websocketTests(true));
  describe("Testing WebStorage in online mode", () => websocketTests(OfflineMode.Off));
} else {
  describe.skip("Skipped websocket related tests. You need to set up account.json", () => {
    it("skipping");
  });
}

function websocketTests(offlineMode: OfflineMode = OfflineMode.On) {

  let store: WebStorage;
  let projs: Project[];
  let files: File[];

  beforeAll(() => {
    Services.setOfflineMode(offlineMode);
    Services.init(null, {
      debugWebSocket: true,
      debugLocalStorage: false,
      debugWebStorage: false,
      debugService: false
    });
    store = Services.storage();
    projs = R.sortBy(prop("id"), map((s: string) => new Project({
      id: offlineMode === OfflineMode.On ? md5(`X${s}`) : `X${s}`,
      name: `X${s}`,
      runs: {},
      last_modified: 0
    }), uniqStrArr(testSize, 20)()));
    files = R.sortBy(prop("id"), map((p: Project) => {
      const name = `default/${uniqStr(20)()}.${J.one_of(["c", "h", "rkt", "txt", "test"])()}`;
      const fid = offlineMode === OfflineMode.On ? md5(p.id + name) : `${p.id}/${name}`;
      const text = uniqStr(5000)();
      return new File({
        id: fid,
        project: p.id,
        name: name,
        contents: text,
        checksum: md5(text),
        last_modified: 0,
        open: false
      });
    }, flatten(repeat(projs, testSize))));
    return Services.login(TestAccount.user, TestAccount.password, false, TestAccount.backend).catch((err) => {
      console.error(err);
    });
  });

  afterAll(() => {
    Services.logout(true);
  });

  async function remoteProjs() {
    let remoteProjs: ProjectBrief[] = await store.getProjects();
    const remote: Project[] = [];
    for (const p of remoteProjs) {
      remote.push(await store.getProject(p.id));
    }
    return R.sortBy(prop("id"), map((p) => ({
      // properties you want to check
      id: p.id,
      name: p.name,
      runs: p.runs,
      last_modified: 0,
    }), remote)) || [];
  }

  async function remoteFiles() {
    let remoteFiles = await store.getAllFiles();
    for (let x of remoteFiles) {
      const file = await store.readFile(x.id);
      Object.assign(x, file);
    }
    remoteFiles = R.sortBy(prop("id"), remoteFiles) || [];
    return map((x) => ({
      id: x.id,
      project: x.project,
      name: x.name,
      contents: x.contents,
      checksum: x.checksum,
      last_modified: 0,
      open: x.open
    }), <File[]> remoteFiles);
  }

  it(`newProject: create ${testSize} projects`, async () => {
    let ids: ProjectID[] = [];
    for (const p of projs) {
      ids.push((await store.newProject(p.name)).id);
    }
    expect(ids).toEqual(R.map(prop("id"), projs));
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });


  it(`getProjects: list all projects`, async () => {
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });

  it("getFileToRun: should return false when no run files", async () => {
    for (const p of projs) {
      const f = await store.getFileToRun(p.id, "default");
      expect(f).toEqual(false);
    }
  });

  it(`newFile: create ${testSize} files per project`, async () => {
    for (const f of files) {
      await store.newFile(f.project, f.name, f.contents);
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });

  it(`getAllFiles: list all files`, async () => {
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
  });

  it(`readFile: read all files`, async () => {
    for (const f of files) {
      const r = await store.readFile(f.id);
      const a = [f.name, f.project, f.contents];
      const b = [r.name, r.project, r.contents];
      expect(a).toEqual(b);
    };
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
  });

  it(`writeFile: update ${halfTestSize} files`, async () => {
    const fs = R.take(halfTestSize, files);
    for (const f of fs) {
      f.contents = uniqStr(5000)();
      f.checksum = md5(f.contents);
      await store.writeFile(f.id, f.contents);
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
  });

  function genRunFiles(p: string): {[index: string]: FileID} {
    return R.zipObj(uniqStrArr(testSize, 30)(),
                    J.array(testSize, J.one_of(map((file) => [p, file.name], files)))());
  }

  it(`setFileToRun: randomly pick a run file per project`, async () => {
    for (const f of files) {
      await store.setFileToRun(f.project, "default", f.name);
      const pj = R.find((x) => x.id === f.project, projs);
      if (offlineMode === OfflineMode.On) {
        pj.runs.default = f.name;
      }
      for (const o of files) {
        const check = await store.getFileToRun(o.project, "default");
        if (o.project ===  f.project) {
          expect(check).toEqual(f.name);
        } else {
          expect(check).not.toEqual(f.name);
        }
      }
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });

  it("set/getSettings: set font_size to 100", async () => {
    const s: Settings = new Settings();
    s.font_size = 100;
    await store.setSettings(s);
    const r = await store.getSettings();
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
        await store.setFileToRun(pj.id, "default", file.name);
        await store.deleteFile(file.id);
        removed.concat(filter((x) => x.id === file.id, files));
        files = filter((x) => x.id !== file.id, files);
        for (const d in pj.runs) {
          if (pj.runs[d] === file.name) {
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
      await store.deleteProject(proj.id);
      projs = R.remove(i, 1, projs);
      files = R.filter((f) => f.project !== proj.id, files);
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });

  it(`renameFile: rename ${halfTestSize} files per project. Should not change the file's id.`, async () => {
    const projGps = R.groupBy((x: File) => x.project, files);
    for (const p in projGps) {
      const pj = R.find((x) => x.id === p, projs);
      console.assert(pj);
      for (const i of R.range(0, Math.min(halfTestSize, projGps[p].length))) {
        const file = projGps[p][i];
        const newname = file.name + "_new_name";
        await store.renameFile(file.id, newname);
        file.name += "_new_name";
      }
    }
    expect(await remoteFiles()).toEqual(expect.arrayContaining(files));
    expect(await remoteProjs()).toEqual(expect.arrayContaining(projs));
  });

  it("syncAll: sync multiple times. Should not crash.", async () => {
    for (const i of R.range(0, 3)) {
      await store.syncAll();
    }
  });
// */
}
