export {LoginError,
        GenericError,
        SyncError}

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

class SyncError extends GenericError {
  constructor(message: string) {
    super(message);
  }
}
