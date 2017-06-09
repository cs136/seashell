import J = require("jscheck");
import {TestAccount} from "./account";
import FakeIndexedDB = require("fake-indexeddb");
import FDBKeyRange = require("fake-indexeddb/lib/FDBKeyRange");
import {File, FileID, FileBrief,
        Project, ProjectID, ProjectBrief,
        Settings, Services} from "../src/helpers/Services";
import * as R from "ramda";

// polyfills
import WebSocket = require("ws");
import * as LS from "localstorage-memory";
(<any>window).localStorage = LS;
(<any>window).WebSocket = WebSocket;
(<any>window).indexedDB = FakeIndexedDB;
(<any>window).IDBKeyRange = FDBKeyRange;

(<any>jasmine).DEFAULT_TIMEOUT_INTERVAL = 80 * 1000;

const LOGIN_URL = "https://www.student.cs.uwaterloo.ca/~cs136/seashell/cgi-bin/login2.cgi";

Services.init(null, {
  debugWebSocket: false,
  debugWebStorage: false,
  debugService: false
});

if (TestAccount.user) {
  describe("Testing Services interface", servicesTest);
} else {
  describe.skip("Skipped websocket related tests. You need to set up account.json", () => {
    it("skipping");
  });
}

function servicesTest() {

  beforeAll(() => {
    return Services.login(TestAccount.user, TestAccount.password, false, LOGIN_URL);
  });

  afterAll(() => {
    Services.logout();
  });

  it("Login multiple times. Should not crash.", async () => {
      // expect(socket.isConnected()).toEqual(true);
      for (const i of R.range(0, 3)) {
        await Services.login(TestAccount.user, TestAccount.password, false, LOGIN_URL);
      }
    });

  it("Logout multiple times. Should not crash.", async () => {
    // expect(socket.isConnected()).toEqual(true);
    for (const i of R.range(0, 3)) {
      await Services.logout();
    }
  });

  it("Login and logout multiple times. Should not crash.", async () => {
    // expect(socket.isConnected()).toEqual(true);
    for (const i of R.range(0, 3)) {
      await Services.logout();
      await Services.login(TestAccount.user, TestAccount.password, false, LOGIN_URL);
    }
  });
};
