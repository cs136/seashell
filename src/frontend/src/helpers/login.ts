import * as $ from "jquery";
export {Login, LoginError, WSConnection}

class LoginError extends Error {
  constructor(msg: string,
              public username?: string,
              public status?: number,
              public statusText?: number) {
    super(msg);
  }
}

class Login {
  public uri: string;
  
  constructor(uri?: string) {
    this.uri = uri || "https://www.student.cs.uwaterloo.ca/~cs136/seashell/cgi-bin/login2.cgi";
  };

  public async login(user: string, password: string, rebootBackend?: boolean): Promise<WSConnection> {
    try {
      const response = await $.ajax({
        url: this.uri,
        type: "POST",
        data: {
          "u": user, 
          "p": password,
          "reset": !! rebootBackend
        },
        dataType: "json"
      });
      return new WSConnection(response.key,
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
  }
}


class WSConnection {
  public wsURI: string;

  constructor(public publicKey: number[], 
              public host: string, 
              public port: number,
              public pingPort: number) {
    this.wsURI = `wss://${this.host}:${this.port}`;
  };
}

