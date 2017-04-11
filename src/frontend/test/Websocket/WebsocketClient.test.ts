import * as E from "../../src/helpers/Errors";
import * as W from "../../src/helpers/Websocket/WebsocketClient";
import * as S from "../../src/helpers/Services";

// polyfills
import WebSocket = require("ws");

(<any>window).WebSocket = WebSocket;

describe("Running tests for WebsocketClient", () => {
  const socket = new W.SeashellWebsocket(true);
  it("When internet is unavaliable, sendRequest should throw NoInternet.", async () => {
    socket.mockInternet = W.MockInternet.Offline;
    expect(() => {
      socket.sendMessage({});
    }).toThrowError(E.NoInternet);
  });
  it("If connection is not authenticated, sendRequest should throw LoginRequired.", async () => {
    try {
      const cnn = new S.Connection("", [], "", 0, 0);
      await socket.connect(cnn);
    } catch (err) {
      if (! (err instanceof E.NoInternet)) {
        throw err;
      }
    }
  });
  it("If websocket is closed, should automatically try to connect.", async () => {

  });
  it("If ping times out, should close websocket and fall back to previous cases.", async () => {

  });
});
