import * as $ from "jquery";
import {SeashellWebsocket} from "./Websocket/WebsocketClient";
import {WebStorage} from "./Storage/WebStorage";
import {LocalStorage} from "./Storage/LocalStorage";
import {AbstractStorage,
        File, FileID, FileBrief,
        Project, ProjectID, ProjectBrief,
        Settings} from "./Storage/Interface";
import {OnlineCompiler} from "./Compiler/OnlineCompiler";
import {OfflineCompiler} from "./Compiler/OfflineCompiler";
import {AbstractCompiler,
        Test,
        CompilerResult,
        CompilerDiagnostic} from "./Compiler/Interface";
import {LoginError, LoginRequired} from "./Errors";
export * from "./Storage/Interface";
export * from "./Compiler/Interface";
export {Services, Connection, DispatchFunction};


class Connection {
  public wsURI: string;

  constructor(public username: string,
              public key: number[],
              public host: string,
              public port: number,
              public pingPort: number) {
    this.wsURI = `wss://${this.host}:${this.port}`;
  };
}

type DispatchFunction = (act: Object) => Object;

namespace Services {
  let connection: Connection;
  let dispatch: DispatchFunction | null = null;
  let socketClient: SeashellWebsocket | null = null;
  let localStorage: LocalStorage | null = null;
  let webStorage: WebStorage | null = null;
  let offlineCompiler: OfflineCompiler | null = null;
  let onlineCompiler: OnlineCompiler | null = null;
  let debug: boolean;

  export function init(disp: DispatchFunction,
                       options?: { debugService?: boolean;
                                   debugWebSocket?: boolean;
                                   debugWebStorage?: boolean;
                                   debugLocalStorage?: boolean; }) {
    dispatch = disp;
    options = options || {};
    debug = options.debugService || false;
    socketClient = new SeashellWebsocket(options.debugWebSocket);
    localStorage = new LocalStorage(options.debugLocalStorage);
    webStorage = new WebStorage(socketClient, localStorage, options.debugWebStorage);
    offlineCompiler = new OfflineCompiler(localStorage, dispatch);
    onlineCompiler = new OnlineCompiler(socketClient, webStorage, offlineCompiler, dispatch);
  }

  export function storage(): WebStorage {
    if (webStorage === null) {
      throw new Error("Must call Services.init() before Services.storage().");
    }
    return webStorage;
  }

  export function compiler(): AbstractCompiler {
    if (onlineCompiler === null) {
      throw new Error("Must call Services.init() before Services.compiler().");
    }
    return onlineCompiler;
  }

  export async function login(user: string,
                              password: string,
                              rebootBackend: boolean = false,
                              uri = "https://www.student.cs.uwaterloo.ca/~cs136/seashell-react/cgi-bin/login2.cgi"): Promise<void> {
    if (!localStorage || !socketClient || !webStorage) {
      throw new Error("Must call Services.init() before Services.login()");
    }
    try {
      debug && console.log("Logging in...");
      let response = await <PromiseLike<any>>$.ajax({
        url: uri,
        type: "POST",
        data: {
          "u": user,
          "p": password,
          "reset": !! rebootBackend
        },
        dataType: "json"
      });
      debug && console.log("Login succeeded.");
      response.user = user; // Save user so that we can log in later.
      window.localStorage.setItem("seashell-credentials", JSON.stringify(response));
      connection = new Connection(user,
                                  response.key,
                                  response.host,
                                  response.port,
                                  response.pingPort);
    } catch (ajax) {
      const status     = ajax.status;
      const code       = ajax.responseJSON.error.code;
      const msg        = ajax.responseJSON.error.message;
      const statusText = ajax.statusText;
      throw new LoginError(`Login failure (${code}): ${msg}`, user, status, statusText);
    }

    // login successful
    await connectWith(connection);
  }

  export async function logout(deleteDB: boolean = false) {
    if (!localStorage || !socketClient) {
      throw new Error("Must call Services.init() before Services.logout()");
    }
    await socketClient.disconnect();
    if (deleteDB) {
      await localStorage.deleteDB();
      debug && console.log("Deleted user's indexedDB.");
    }
    window.localStorage.removeItem("seashell-credentials");
    debug && console.log("User logged out.");
  }

  export async function autoConnect() {
    if (!localStorage || !socketClient || !webStorage) {
      throw new Error("Must call Services.init() before Services.login()");
    }
    const credstring = window.localStorage.getItem("seashell-credentials");
    if (credstring) {
      const credentials = JSON.parse(credstring);
      let connection = new Connection(credentials.user,
                                      credentials.key,
                                      credentials.host,
                                      credentials.port,
                                      credentials.pingPort);
      // login successful
      return await connectWith(connection);
    } else {
      throw new LoginRequired();
    }
  }

  async function connectWith(connection: Connection) {
    if (!localStorage || !socketClient || !webStorage) {
      throw new Error("Must call Services.init() before Services.login()");
    }

    await localStorage.connect(`seashell8-${connection.username}`);
    await socketClient.connect(connection);
    await webStorage.syncAll();
    return connection.username;
  }
}
