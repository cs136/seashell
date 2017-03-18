import * as $ from "jquery";
export {Login, LoginError, Connection}

class LoginError extends Error {
  constructor(msg: string,
              public username?: string,
              public status?: number,
              public statusText?: number) {
    super(msg);
  }
}

class Login {
  public static async login(user: string,
                            password: string,
                            rebootBackend?: boolean,
                            uri?: string): Promise<Connection> {
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
      return new Connection(user,
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

