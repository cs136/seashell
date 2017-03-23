import * as $ from "jquery";
import {SeashellWebsocket} from "./Websocket/WebsocketClient";
import {WebStorage} from "./Storage/WebStorage";
import {LocalStorage} from "./Storage/LocalStorage";
import {AbstractStorage,
        File, FileID, FileBrief,
        Project, ProjectID, ProjectBrief,
        Settings, defaultSettings} from "./Storage/Interface";
import {OnlineCompiler} from "./Compiler/OnlineCompiler";
import {OfflineCompiler} from "./Compiler/OfflineCompiler";
import {AbstractCompiler,
        Test,
        CompilerResult,
        CompilerMessage} from "./Compiler/Interface";

export * from "./Storage/Interface";
export * from "./Compiler/Interface";
export {Services, GenericError, LoginError, Connection, DispatchFunction};

class GenericError {
  msg: string;
  type: any;
  constructor(msg: string, type: any) {
    this.msg = msg;
    this.type = type;
  }
}

class LoginError extends GenericError {
  constructor(msg: string,
              public username?: string,
              public status?: number,
              public statusText?: number) {
    super(msg, LoginError);
  }
}

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
  let dispatch: DispatchFunction = null;
  let socketClient: SeashellWebsocket = null;
  let localStorage: LocalStorage = null;
  let webStorage: WebStorage = null;
  let offlineCompiler: OfflineCompiler = null;
  let onlineCompiler: OnlineCompiler = null;

  export function init(disp: DispatchFunction) {
    dispatch = disp;
    socketClient = new SeashellWebsocket();
    localStorage = new LocalStorage();
    webStorage = new WebStorage(socketClient, localStorage);
    offlineCompiler = new OfflineCompiler(localStorage, dispatch);
    onlineCompiler = new OnlineCompiler(socketClient, webStorage, offlineCompiler, dispatch);
  }

  export function storage(): WebStorage {
    if (webStorage === null) {
      throw new LoginError("Must call Services.init() before Services.storage().");
    }
    return webStorage;
  }

  export function compiler(): AbstractCompiler {
    if(onlineCompiler === null) {
      throw new LoginError("Must call Services.init() before Services.compiler().");
    }
    return onlineCompiler;
  }

  export async function login(user: string,
                              password: string,
                              rebootBackend?: boolean,
                              uri?: string): Promise<void> {
    uri = uri || "https://www.student.cs.uwaterloo.ca/~cs136/seashell/cgi-bin/login2.cgi";
    try {
      const response = await $.ajax({
        url: uri,
        type: "POST",
        data: {
          "u": user,
          "p": password,
          "reset": !! rebootBackend
        },
        dataType: "json"
      });
      connection = new Connection(user,
                                  response.key,
                                  response.host,
                                  response.port,
                                  response.pingPort);
    } catch (ajax) {
      const code       = ajax.responseJSON.error.code;
      const msg        = ajax.responseJSON.error.message;
      const status     = ajax.status;
      const statusText = ajax.statusText;
      throw new LoginError(`Login failure (${code}): ${msg}`, user, status, statusText);
    }

    // login successful
    await localStorage.connect(`seashell-${connection.username}`);
    await socketClient.connect(connection);
  }

  export async function logout() {
    await socketClient.disconnect();
  }
}
