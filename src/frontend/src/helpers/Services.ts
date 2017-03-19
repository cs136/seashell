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
export {Services, LoginError, Connection};

class LoginError extends Error {
  constructor(msg: string,
              public username?: string,
              public status?: number,
              public statusText?: number) {
    super(msg);
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

namespace Services {
  let connection: Connection;
  const socketClient: SeashellWebsocket = new SeashellWebsocket();
  const localStorage: LocalStorage = new LocalStorage();
  const webStorage: WebStorage = new WebStorage(socketClient, localStorage);
  const offlineCompiler: OfflineCompiler = new OfflineCompiler();
  const onlineCompiler: OnlineCompiler = new OnlineCompiler(socketClient, webStorage, offlineCompiler);

  export function storage(): WebStorage {
    return webStorage;
  }

  export function compiler(): AbstractCompiler {
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
    await socketClient.authenticate(connection);
    await webStorage.connect();
  }

  export async function logout() {
    await socketClient.close();
  }
}
