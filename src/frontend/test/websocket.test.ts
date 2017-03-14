import "jest";
import {WebsocketService, 
        WSConnection, 
        SeashellWebsocket, 
        Storage, 
        File, 
        Project} from "../src/helpers/websocket";
import {Login, PublicKey} from "../src/helpers/login";
import * as R from "ramda";
import FakeIndexedDB = require("fake-indexeddb");
import FDBKeyRange = require("fake-indexeddb/lib/FDBKeyRange");
import {map, filter, flatten, repeat, head, prop} from "ramda";
import J = require("jscheck");
import {TestAccount} from "./account";

jasmine.DEFAULT_TIMEOUT_INTERVAL = 30*1000;

const testSize = 5;
const halfTestSize = Math.floor(testSize/2);

let unique = 0;
// should return a thunk, see jscheck documentation 
function uniqStr(len: number): () => string {
  return () => {
    return J.string(J.number(0, len), J.character("M"))() + (++unique);
  };
}

function uniqStrArr(arrLen: number, strLen: number): () => string[] {
  return J.array(arrLen, uniqStr(strLen));
}

describe("testing websocket.ts", () => {

  let socket: WebsocketService;

  beforeAll(() => {
    const store = new Storage(TestAccount.user, {
      IDBKeyRange: FDBKeyRange,
      indexedDB: FakeIndexedDB
    });

    const login = new Login("https://www.student.cs.uwaterloo.ca/~cs136/seashell/cgi-bin/login2.cgi");
    return login.login(TestAccount.user, TestAccount.password, true).then((cnn) => {
      socket = new WebsocketService(cnn, store);
    }).then(() => {
      return socket.connect();
    }).catch((err) => {
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

  let projs: Project[] = R.sortBy(prop("name"), map((s: string) => {
    return {
      name: s,
      runs: {}
    }
  }, uniqStrArr(testSize,30)()));

  let files: File[] = map((p) => {
    return {
      project: p.name,
      name: uniqStr(30)(),
      contents: uniqStr(5000)()
    };
  }, flatten(repeat(projs, testSize)));

  async function expectProjectsToEq(exp: Project[]) {
    const ls: Project[] = await socket.getProjects();
    const dbProjs: Project[] = R.sortBy(prop("name"), map((p) => {
      return {
        name: p.name,
        runs: p.runs
      }
    }, projs));
    expect(dbProjs).toEqual(exp);
  }

  async function expectFilesToEq(files: File[]) {
    let projGps = R.groupBy((x:File) => x.project, files);
    for (const p of projs) {
      let dbFiles: File[] = await socket.listProject(p.name);
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

  it(`getProject: list all projects`, async () => {
    await expectProjectsToEq(projs);
  });

  it.skip("writeFile" , async() => {
  });
});

