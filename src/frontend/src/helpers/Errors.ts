import * as W from "./Websocket/Interface";
import * as S from "./Storage/Interface";
export {LoginError,
        GenericError,
        WebsocketError,
        CompilerError,
        SkeletonError,
        StorageError,
        ConflictError,
        LoginRequired,
        RequestError,
        NoInternet,
        RequestAborted,
        RequestTimedOut};

class GenericError extends Error {
    constructor(message?: string) {
        const trueProto = new.target.prototype;
        super(message);
        Object.setPrototypeOf(this, trueProto);
    }
}

class LoginError extends GenericError {
  constructor(message: string = "LoginError",
              public username?: string,
              public status?: number,
              public statusText?: number) {
    super(message);
  }
}

class LoginRequired extends GenericError {
  constructor(message: string = "LoginRequired") {
    super(message);
  }
}

class NoInternet extends GenericError {
  constructor(message: string = "NoInternet") {
    super(message);
  }
}

class RequestAborted extends GenericError {
  constructor(message: string = "RequestAborted") {
    super(message);
  }
}

class RequestError extends GenericError {
  constructor(public message: string = "RequestError",
              public request: W.Request<any>,
              public response: W.Response) {
    super(message);
  }
}
class RequestTimedOut extends GenericError {
  constructor(public message: string = "RequestTimedOut") {
    super(message);
  }
}

class WebsocketError extends GenericError {
  constructor(message: string = "WebsocketError",
              public data?: Object) {
    super(message);
  }
}

class CompilerError extends GenericError {
  constructor(message: string = "CompilerError",
              public data?: Object) {
    super(message);
  }
}

class SkeletonError extends GenericError {
  constructor(message: string = "SkeletonError",
              public data?: Object) {
    super(message);
  }
}

class StorageError extends GenericError {
  constructor(message: string = "StorageError",
              public data?: Object) {
    super(message);
  }
}

class ConflictError extends GenericError {
  constructor(filename: string, results: S.File[]) {
    super(`Conflict occurred in file ${filename}.`);
  }
}
