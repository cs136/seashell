import "jest";
import {LocalStorage} from "../../src/helpers/Storage/LocalStorage";
import {File, FileID, FileBrief,
        Project, ProjectID, ProjectBrief,
        Settings, defaultSettings} from "../../src/helpers/Services";
import J = require("jscheck");
import FakeIndexedDB = require("fake-indexeddb");
import FDBKeyRange = require("fake-indexeddb/lib/FDBKeyRange");
import Dexie from "dexie";
import {map, filter, flatten, repeat, head, prop} from "ramda";
import R = require("ramda");
import md5 = require("md5");

jasmine.DEFAULT_TIMEOUT_INTERVAL = 10*1000;

const testSize = 5;
const halfTestSize = Math.ceil(testSize/2);

let unique = 0;
// should return a thunk, see jscheck documentation
function uniqStr(len: number): () => string {
  return () => J.string(J.number(0, len), J.character())() + (++unique);
}

function uniqStrArr(arrLen: number, strLen: number): () => string[] {
  return J.array(arrLen, uniqStr(strLen));
}

describe("Testing LocalStorage.ts", () => {

  const store = new LocalStorage({
    IDBKeyRange: FDBKeyRange,
    indexedDB: FakeIndexedDB
  });

  store.connect("test");

  let projs: Project[] = R.sortBy(prop("name"), map((s: string) => ({
    id: s,
    name: s,
    runs: {}
  }), uniqStrArr(testSize,30)()));

  let files: File[] = map((p:Project) => {
    const name = uniqStr(30)();
    return {
      id: <[string,string]>[p.id, name],
      project: p.name,
      name: name,
      contents: uniqStr(5000)()
    }
  }, flatten(repeat(projs, testSize)));

  it(`newProject: create ${testSize} projects`, async () => {
    for (const p of projs) {
      await store.newProject(p.name);
    }
    await expectProjectsToEq(projs);
  });

  async function expectProjectsToEq(exp: Project[]) {
    const ls: Project[] = await store.getProjects();
    const dbProjs: Project[] = R.sortBy(prop("name"), map((p) => ({
      id: p.id,
      name: p.name,
      runs: p.runs
    }), ls));
    expect(dbProjs).toEqual(exp);
  }

  it(`getProjects: list all projects`, async () => {
    await expectProjectsToEq(projs);
  });

  it(`getFileToRun: should return undefined when no run files`, async () => {
    for (const p of projs) {
      for (const d in p.runs) {
        const f = await store.getFileToRun(p.name, d);
        expect(f).toEqual(undefined);
      }
    }
  });

  it(`newFile: create ${testSize} files per project`, async () => {
    for (const f of files) {
      const id = await store.newFile(f.project, f.name, f.contents);
      f.id = id;
    }
    await expectFilesToEq(files);
    await expectProjectsToEq(projs);
  });

  async function expectFilesToEq(files: File[]) {
    let projGps = R.groupBy((x:File) => x.project, files);
    for (const p of projs) {
      let dbFiles: File[] = await store.getFiles(p.name);
      let projFiles       = projGps[p.name];
      dbFiles   = R.sortBy(prop("name"), dbFiles);
      projFiles = R.sortBy(prop("name"), projGps[p.name]);
      expect(map((x) => {
        return {
          id: x.id,
          project: x.project,
          name: x.name,
          contents: x.contents,
        }
      }, dbFiles)).toEqual(projFiles || []);
    }
  }

  it(`getFiles: list files per project`, async () => {
    await expectFilesToEq(files);
  });

  it(`readFile: read all files`, async () => {
    for (const f of files) {
      const r = await store.readFile([f.project, f.name]);
      const a = [f.name, f.project, f.contents];
      const b = [r.name, r.project, r.contents];
      expect(a).toEqual(b);
    };
  });

  it(`writeFile: update ${halfTestSize} files`, async () => {
    const fs = R.take(halfTestSize, files);
    for (const f of fs) {
      f.contents = uniqStr(5000)();
      await store.writeFile([f.project, f.name], f.contents);
      await expectFilesToEq(files);
    }
  });

  function genRunFiles(p: string) : {[index: string]: FileID} {
    return R.zipObj(uniqStrArr(testSize,30)(),
                    J.array(testSize, J.one_of(map((file) => [p, file.name], files)))())
  }

  it(`setFileToRun: randomly pick a run file per project`, async () => {
    for (const f of files) {
      const id: [string, string] = [f.project, f.name];
      const dir = uniqStr(30)();
      await store.setFileToRun(f.project, dir, id);
      const proj = R.find((p) => f.project == p.id, projs);
      proj.runs[dir] = id;
      await expectProjectsToEq(projs);
    }
  });

  // it(`updateProject: update run files`, async () => {
  //   for (const p of projs) {
  //     p.runs = genRunFiles(p.name);
  //     await store.updateProject(p);
  //   }
  //   await expectProjectsToEq(projs);
  // });

  it(`deleteFile: delete ${halfTestSize} files per project, should also remove from run files`, async () => {
    const projGps = R.groupBy((x:File) => x.project, files);
    for (const p in projGps) {
      const pj = R.find((x) => x.name == p, projs);
      for (const i of R.range(0,Math.min(halfTestSize, projGps[p].length))) {
        const nm = projGps[p][i].name;
        const fid: [string, string] = [p, nm];
        await store.setFileToRun(pj.name, nm, fid);
        await store.deleteFile(fid);
        files = filter((x) => x.project != p || x.name != nm, files);
        for (const d in pj.runs) {
          if (R.equals(pj.runs[d], fid)) {
            delete pj.runs[d];
          }
        }
        await expectProjectsToEq(projs);
        await expectFilesToEq(files);
      }
    }
  });

  it(`renameFile: rename ${halfTestSize} file per project, should also rename run files`, async () => {
    const projGps = R.groupBy((x:File) => x.project, files);
    for (const p in projGps) {
      const pj = R.find((x) => x.name == p, projs);
      for (const i of R.range(0,Math.min(halfTestSize, projGps[p].length))) {
        const oldname = projGps[p][i].name;
        const newname = (projGps[p][i].name += "_new_name");
        const fid: [string, string] = [p, oldname];
        for (const d in pj.runs) {
          if (R.equals(pj.runs[d], fid)) {
            pj.runs[d][1] = newname;
          }
        }
        await store.renameFile(fid, newname);
        await expectFilesToEq(files);
        await expectProjectsToEq(projs);
      }
    }
  });

  it(`deleteProject: delete ${halfTestSize} projects, should also delete children`, async () => {
    for (const i of R.range(0,halfTestSize)) {
      const p = projs[0].name;
      await store.deleteProject(projs[0].name);
      projs = R.tail(projs);
      files = R.filter((f) => f.project != p, files);
      await expectProjectsToEq(projs);
      await expectFilesToEq(files);
    }
  });

  it("getSettings: should return undefined when unset", async () => {
    const s = await store.getSettings();
    expect(s).toEqual(undefined);
  });

  it("setSettings: set random properties", async () => {
    const s: Settings = defaultSettings;
    s.fontSize = 100;
    await store.setSettings(s);
    const r = await store.getSettings();
    expect(r).toEqual(s);
  });

  it("checking final state" , async () => {
    await expectProjectsToEq(projs);
    await expectFilesToEq(files);
  });

});


