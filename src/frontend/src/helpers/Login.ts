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
      try {
        return Connection.getInstance();
      } catch(ex) {
        if(ex instanceof LoginError) {
          return Connection.initialize(user,
                                       response.key,
                                       response.host,
                                       response.port,
                                       response.pingPort);
        }
        else throw ex;
      }

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

  private constructor(public username: string,
              public key: number[],
              public host: string,
              public port: number,
              public pingPort: number) {
    this.wsURI = `wss://${this.host}:${this.port}`;
  };

  public static instance: Connection = null;

  public static getInstance(): Connection {
    if(Connection.instance !== null) {
      return Connection.instance;
    }
    else {
      throw new LoginError("Called Connection.getInstance() before logging in.");
    }
  }

  public static initialize(username: string, key: number[], host: string, port: number, pingPort: number): Connection {
    if(Connection.instance !== null) {
      throw new LoginError("Connection initialized twice.");
    }
    Connection.instance = new Connection(username, key, host, port, pingPort);
    return Connection.instance;
  }
}

