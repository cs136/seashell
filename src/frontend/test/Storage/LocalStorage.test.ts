import "jest";
import {LocalStorage} from "../../src/helpers/Storage/LocalStorage";
import {File, FileID,
        Project, ProjectID, Contents,
        Settings} from "../../src/helpers/Services";
import J = require("jscheck");
import FakeIndexedDB = require("fake-indexeddb");
import FDBKeyRange = require("fake-indexeddb/lib/FDBKeyRange");
import Dexie from "dexie";
import {map, filter, flatten, repeat, head, prop} from "ramda";
import R = require("ramda");
import md5 = require("md5");
import * as LS from "localstorage-memory";

(<any>window).localStorage = LS;
(<any>window).indexedDB = FakeIndexedDB;
(<any>window).IDBKeyRange = FDBKeyRange;

// const testSize = 5; // must be >= 5
// const halfTestSize = Math.ceil(testSize / 2);

// let unique = 0;
// // should return a thunk, see jscheck documentation
// function uniqStr(len: number): () => string {
//   return () => J.string(J.number(0, len), J.character())() + (++unique);
// }

// function uniqStrArr(arrLen: number, strLen: number): () => string[] {
//   return J.array(arrLen, uniqStr(strLen));
// }

describe("Testing LocalStorage interface functions", () => {

  let store;

  beforeEach(async () => {
    store = new LocalStorage(false, () => false);
    await store.connect("LocalStorage.test.ts");
  });

  afterEach(async () => {
    await store.deleteDB();
  });

  it(`newProject`, async () => {
    const p = await store.newProject("test");
    const j = await store.getProject(p.id);
    expect(p).toEqual(j);
  });

  it(`getProjects`, async () => {
    const ls = ["personal", "A0", "T0", "A10", "T10"];
    for (const n of ls) {
      await store.newProject(n);
    };
    const lsp = (await store.getProjects()).map((o) => o.name);
    expect(lsp).toEqual(expect.arrayContaining(ls));
  });

  it(`getProject`, async () => {
    const p = await store.newProject("test");
    const j = await store.getProject(p.id);
    expect(p).toEqual(j);
  });

  it(`deleteProjects`, async () => {
    const ls = ["personal", "A0", "T0", "A10", "T10"];
    const projs = [];
    for (let n of ls) {
      projs.push(await store.newProject(n));
    };
    await store.deleteProject(projs[2].id);
    const lsp = (await store.getProjects()).map((o) => o.name);
    expect(lsp).toEqual(expect.arrayContaining(["personal", "A0", "A10", "T10"]));
  });

  it(`updateLastUsed`, async () => {
    const ls = ["personal", "A0", "T0", "A10", "T10"];
    const projs = [];
    for (let n of ls) {
      projs.push(await store.newProject(n));
    };
    await store.updateLastUsed(projs[2].id);
    const lsp = (await store.getProjects()).map((o) => o.name);
    expect(lsp[0]).toEqual("T0");
  });

  it(`newFile`, async () => {
    const p = await store.newProject("project");
    const f = await store.newFile(p.id, "file");
    const c = await store.readFile(f.id, true);
    expect((c.contents as Contents).contents).toEqual("");
  });

  it(`getFiles`, async () => {
    const p = await store.newProject("project");
    const fs = ["file1", "file2", "file3"];
    for (const f of fs) {
      await store.newFile(p.id, f);
    }
    const got = (await store.getFiles(p.id)).map(R.prop("name"));
    expect(got).toEqual(expect.arrayContaining(fs));
  });

  it(`getAllFiles`, async () => {
    const p1 = await store.newProject("project1");
    const p2 = await store.newProject("project2");
    const fs = ["file1", "file2", "file3"];
    for (const f of fs) {
      await store.newFile(p1.id, f);
      await store.newFile(p2.id, f);
    }
    const got = (await store.getAllFiles()).map(R.prop("name"));
    expect(got).toEqual(expect.arrayContaining(fs.concat(fs)));
  });

  it(`readFile`, async () => {
    const p = await store.newProject("project");
    const f = await store.newFile(p.id, "file", "contents");
    const c = await store.readFile(f.id, true);
    expect((c.contents as Contents).contents).toEqual("contents");
  });

  it(`getFileByName`, async () => {
    const p = await store.newProject("project");
    const f = await store.newFile(p.id, "file", "contents");
    const got = (await store.getFileByName(p.id, "file", true));
    expect((got.contents as Contents).contents).toEqual("contents");
  });

  it(`deleteFile`, async () => {
    const p = await store.newProject("project");
    const f = await store.newFile(p.id, "file", "contents");
    await store.deleteFile(p.id, "file");
    const got = await store.getFiles(p.id);
    expect(got).toEqual([]);
  });

  it(`renameFile`, async () => {
    const p = await store.newProject("project");
    const f = await store.newFile(p.id, "file", "contents");
    const nf = await store.renameFile(p.id, "file", "newFile");
    const got = (await store.getFiles(p.id)).map(prop("name"));
    expect(got).toEqual(["newFile"]);
    const c = await store.readFile(nf.id, true);
    expect((c.contents as Contents).contents).toEqual("contents");
  });

  it(`writeFile`, async () => {
    const p = await store.newProject("project");
    const f = await store.newFile(p.id, "file", "contents");
    const nid = await store.writeFile(f.id, "newContents");
    const nf = await store.readFile(nid);
    expect((nf.contents as Contents).contents).toEqual("newContents");
  });

  it(`getVersions`, async () => {
    const p = await store.newProject("project");
    let fid = (await store.newFile(p.id, "file")).id;
    fid = await store.writeFile(fid, "contents");
    fid = await store.writeFile(fid, "newContents");
    const versions = (await store.getVersions(p.id, "file")).map(R.prop("contents"));
    expect(versions).toEqual(expect.arrayContaining(["", "contents", "newContents"]));
  });

  it(`getQuestions`, async () => {
    const p = await store.newProject("project");
    await store.newFile(p.id, "q1/file");
    expect(await store.getQuestions(p.id)).toEqual([]);
  });

  it(`getFileToRun`, async () => {
    const p = await store.newProject("project");
    await store.newQuestion("question");
    expect(await store.getFileToRun(p.id, "question")).toEqual(false);
  });

  it(`setFileToRun`, async () => {
    const p = await store.newProject("project");
    await store.newQuestion("question");
    await store.newFile(p.id, "fileToRun");
    await store.setFileToRun(p.id, "question", "fileToRun")    ;
    expect(await store.getFileToRun(p.id, "question")).toEqual("fileToRun");
  });

  it(`getSettings`, async () => {
    const s = await store.getSettings();
    expect(s).toEqual(new Settings());
  });


  it(`setSettings`, async () => {
    const s = new Settings();
    s.font_size++;
    await store.setSettings(s);
    const got = await store.getSettings();
    expect(got).toEqual(s);
  });

  it(`newQuestion`, async () => {
    const p = await store.newProject("project");
    const ls = ["q1", "p1", "q0", "p0"];
    for (const n of ls) {
      await store.newQuestion(p.id, n);
    };
    const lsp = await store.getQuestions(p.id);
    expect(lsp).toEqual(expect.arrayContaining(ls));
  });

  it(`deleteQuestion`, async () => {
    const p = await store.newProject("project");
    for (const n of ["q1", "p1", "q0", "p0"]) {
      await store.newQuestion(p.id, n);
    };
    await store.deleteQuestion(p.id, "p1");
    expect(await store.getQuestions(p.id))
      .toEqual(expect.arrayContaining(["q1", "q0", "p0"]));
  });

  it("getOpenFiles", async () => {
    const p = await store.newProject("project");
    await store.newQuestion("question");
    expect(await store.getOpenFiles(p.id, "question")).toEqual([]);
  });

  it("addOpenFile", async () => {
    const p = await store.newProject("project");
    await store.newQuestion("question");
    await store.newFile(p.id, "openFile1");
    await store.newFile(p.id, "openFile2");
    await store.addOpenFile(p.id, "question", "openFile1");
    await store.addOpenFile(p.id, "question", "openFile2");
    expect(await store.getOpenFiles(p.id, "question")).toEqual(["openFile1", "openFile2"]);
  });

  it("removeOpenFile", async () => {
    const p = await store.newProject("project");
    await store.newQuestion("question");
    await store.newFile(p.id, "openFile1");
    await store.newFile(p.id, "openFile2");
    await store.addOpenFile(p.id, "question", "openFile1");
    await store.addOpenFile(p.id, "question", "openFile2");
    await store.removeOpenFile(p.id, "question", "openFile2");
    expect(await store.getOpenFiles(p.id, "question")).toEqual(["openFile1"]);
  });

  it("exportAsZip", async () => {
    const p = await store.newProject("project");
    await store.newQuestion("question");
    await store.newFile(p.id, "openFile1");
    await store.newFile(p.id, "openFile2");
    const project = await store.exportAsZip(p.id);
    const question = await store.exportAsZip(p.id, "question");
    expect(project).toBeDefined();
    expect(question).toBeDefined();
  });

});
