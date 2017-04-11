import J = require("jscheck");
import {TestAccount} from "./account";
import FakeIndexedDB = require("fake-indexeddb");
import FDBKeyRange = require("fake-indexeddb/lib/FDBKeyRange");
import {File, FileID, FileBrief,
        Project, ProjectID, ProjectBrief,
        Settings, defaultSettings, Services} from "../src/helpers/Services";
import * as R from "ramda";

// polyfills
import WebSocket = require("ws");

(<any>window).WebSocket = WebSocket;
(<any>window).indexedDB = FakeIndexedDB;
(<any>window).IDBKeyRange = FDBKeyRange;

(<any>jasmine).DEFAULT_TIMEOUT_INTERVAL = 80 * 1000;

Services.init(null, {
  debugWebSocket: false,
  debugWebStorage: false,
  debugService: false
});

describe("Testing WebStorage interface", () => {

  beforeAll(() => {
    return Services.login(TestAccount.user, TestAccount.password, false, TestAccount.backend);
  });

  afterAll(() => {
    Services.logout();
  });

  it("Login multiple times. Should not crash.", async () => {
      // expect(socket.isConnected()).toEqual(true);
      for (const i of R.range(0, 3)) {
        await Services.login(TestAccount.user, TestAccount.password, false, TestAccount.backend);
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
      await Services.login(TestAccount.user, TestAccount.password, false, TestAccount.backend);
    }
  });
});
