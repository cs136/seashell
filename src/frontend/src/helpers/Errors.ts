import * as W from "./Websocket/Interface";
export {LoginError,
        GenericError,
        WebsocketError,
        CompilerError,
        LoginRequired,
        RequestError};

class GenericError extends Error {
  __proto__: Error;
    constructor(message?: string) {
        const trueProto = new.target.prototype;
        super(message);
        this.__proto__ = new.target.prototype;
    }
}

class LoginError extends GenericError {
  constructor(message: string,
              public username?: string,
              public status?: number,
              public statusText?: number) {
    super(message);
  }
}

class LoginRequired extends GenericError {
  constructor() {
    super();
  }
}

class RequestError extends GenericError {
  constructor(public message: string,
              public request: W.Request<any>,
              public response: W.Response) {
    super(message);
  }
}

class WebsocketError extends GenericError {
  constructor(message: string,
              public data?: Object) {
    super(message);
  }
}

class CompilerError extends GenericError {
  constructor(message: string,
              public data?: Object) {
    super(message);
  }
}
