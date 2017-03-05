import "jest";
import {Store, File, Project} from "../src/helpers/storage";
import J = require("jscheck");
import FakeIndexedDB = require("fake-indexeddb");
import FDBKeyRange = require("fake-indexeddb/lib/FDBKeyRange");
import Dexie from "dexie";
import {map, filter, flatten, repeat, head, prop} from "ramda";
import R = require("ramda");
import md5 = require("md5");

let unique = 0;

function uniqStr(len: number): () => string {
  return () => {
    return J.string(J.number(0, len), J.character())() + (++unique);
  };
}

describe("Testing storage.js", () => {

  const store: Store = new Store("y667li", {
    IDBKeyRange: FDBKeyRange,
    indexedDB: FakeIndexedDB
  });

  let projs: Project[] = R.sortBy(prop("name"), map((s: string) => {
    return {
      name: s
    }
  }, J.array(10, uniqStr(30))()));

  let files: File[] = map((p) => {
    return {
      project: p.name,
      name: uniqStr(30)(),
      contents: uniqStr(5000)()
    };
  }, flatten(repeat(projs, 10));

  it("newProject: create 10 projects", async () => {
    for (const p of projs) {
      await store.newProject(p.name);
    }
  });

  it("updateProject: update last_visited", async () => {
    for (const p of projs) {
      await store.updateProject(p);
    }
  });

  async function expectProjectsToBe(exp: Project[]) {
    const ls: Project[] = await store.getProjects();
    const dbProjs = map((f:Project) => {
      return {name: f.name}
    }, ls.sort());
    expect(dbProjs).toEqual(exp);
  }

  it("getProjects: list all projects", async () => {
    await expectProjectsToBe(projs);
  });

  it("newFile: create 100 files", async () => {
    for (const f of files) {
      await store.newFile(f.project, f.name, f.contents, '', true, '');
    }
  });

  async function expectFilesToBe(files: File[]) {
    let projGps = R.groupBy((x:File) => x.project, files);
    for (const p of projs) {
      let dbFiles: File[] = await store.listProject(p.name);
      let projFiles       = projGps[p.name];
      dbFiles   = R.sortBy(prop("name"), dbFiles);
      projFiles = R.sortBy(prop("name"), projGps[p.name]);
      expect(map((x) => {
        return {
          project: x.project,
          name: x.name,
          contents: x.contents,
        }
      }, dbFiles)).toEqual(projFiles);
    }
  }

  it("listProject: list files per project", async () => {
    await expectFilesToBe(files);
  });

  it("readFile: read all files", async () => {
    for (const f of files) {
      const r = await store.readFile(f.project, f.name);
      const a = [f.name, f.project, f.contents];
      const b = [r.name, r.project, r.contents];
      expect(a).toEqual(b);
    };
  });

  it("writeFile: update all files", async () => {
    for (const f of files) {
      f.contents = uniqStr(5000)();
      const s = await store.writeFile(f.project, f.name, f.contents, "", "");
      const r = await store.readFile(f.project, f.name);
      const a = [f.name, f.project, f.contents];
      const b = [r.name, r.project, r.contents];
      expect(a).toEqual(b);
    }
  });

  it("deleteFile: delete 2 files per project", async () => {
    const projGps = R.groupBy((x:File) => x.project, files);
    for (const p in projGps) {
      const nm1 = projGps[p][0].name;
      const nm2 = projGps[p][1].name;
      await store.deleteFile(p, nm1, false);
      await store.deleteFile(p, nm2, false);
      files = filter((x:File) => x.project != p || (x.name != nm1 && x.name != nm2), files);
      await expectProjectsToBe(projs);
      await expectFilesToBe(files);
    }
  });

  it("deleteProject: delete 5 projects and their children", async () => {
    for (const i of R.range(0,5)) {
      const p = projs[0].name;
      await store.deleteProject(projs[0].name, false);
      projs = R.tail(projs);
      files = R.filter((f) => f.project != p, files);
      await expectFilesToBe(files);
      await expectProjectsToBe(projs);
    }
  });

});


