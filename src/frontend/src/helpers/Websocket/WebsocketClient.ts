import {AbstractCoder, Coder, ShittyCoder} from "./Crypto";
import {Connection} from "../Services";
import * as R from "ramda";
import * as E from "../Errors";
import {Message, Request, Response, Callback} from "./Interface";
import {appStateActions} from "../../reducers/appStateReducer";

export {SeashellWebsocket, MockInternet}

enum MockInternet {Online, Offline, Real};

class SeashellWebsocket {
  private connection?: Connection;
  private coder: AbstractCoder;
  private websocket?: WebSocket;
  private lastMsgID: number;
  public requests: {[index: number]: Request<any>};
  private failed: boolean; // not used?
  private closes: () => void;
  private failures: () => void;
  public debug: boolean; // toggle console.log for tests
  private pingLoop: any;
  private callbacks: Callback[];
  public mockInternet: MockInternet;
  // this allows WebsocketService to access member functions by string key
  // [key: string]: any;

  constructor(debug?: boolean) {
    this.debug = debug || false;
    this.callbacks = [];
  }

  // Connects and authenticates the socket, sets up the disconnection monitor
  // Pass a new Connection object to overwrite the previously held one
  // It must be safe to call this function consecutively many times
  public async connect(cnn: Connection): Promise<void> {
    this.debug && console.log("Connecting to websocket...");
    this.lastMsgID = 0;
    this.requests = {};
    this.requests[-1] = new Request({id: -1}); // server challenge
    this.requests[-2] = new Request({id: -2}); // reply challenge
    this.requests[-3] = new Request({id: -3});
    this.requests[-4] = new Request({id: -4});
    this.requests[-3].callback = this.io_cb();
    this.requests[-4].callback = this.test_cb();
    // if there's an exisitng websocket,
    // if it's connection or open: do nothing
    // if it's closing or closed: schedule to open a new connection
    if (this.websocket) {
      let websocket = this.websocket;
      switch (websocket.readyState) {
        case websocket.CONNECTING: {
          this.debug && console.log("Socket is already connecting. Action ignored.");
          return;
        }
        case websocket.OPEN: {
          this.debug && console.log("Socket is already connected. Action ignored.");
          return;
        }
        case websocket.CLOSING: {
          this.debug && console.log(`Existing websocket is closing. Wait to reopen new connection.`);
          const promise = new Promise<void>((accept, reject) => {
            websocket.onclose = () => {
              this.connect(cnn).then(accept);
            };
          });
          return promise;
        }
        case websocket.CLOSED: {
          this.debug && console.log(`Existing websocket is closed. Reopening new connection.`);
        }
      }
    }

    this.connection = cnn;
    this.coder = new Coder(this.connection.key);

    try {
      this.websocket = new WebSocket(this.connection.wsURI);
    } catch (err) {
      console.error(`Could not create WebSocket connection to ${this.connection.wsURI}:\n${err}`);
      throw new E.LoginRequired(); // simply ask user to retry
    }

    // Websocket.onclose should race against authentication
    this.websocket.onclose = (evt: CloseEvent) => {
      console.warn(`Websocket lost connection. Trying to reconnect...`);
      clearInterval(this.pingLoop);
      // automatically reconnect after 3s
      if (this.connection) {
        let connection = this.connection;
        setTimeout(() => {
          this.connect(connection);
        }, 3000);
      }
      this.invoke_cb("disconnected");
      for (const i in this.requests) {
        if (evt.code === 4000) {
          this.requests[i].reject(new E.WebsocketError(evt.reason));
          delete this.requests[i];
        } else {
          this.requests[i].reject(new E.NoInternet());
          delete this.requests[i];
        }
      }
    };

    this.websocket.onerror = (err) => {
      console.error(`Websocket encountered error. Closing websocket.`);
      if (this.websocket) {// Always reachable
        this.websocket.close(4000, err.toString());
      }
    };

    this.websocket.onopen = () => {
      let timeoutCount = 0;
      if (this.pingLoop) {
        clearInterval(this.pingLoop);
      }
      this.pingLoop = setInterval(async () => {
        timeoutCount++;
        if (timeoutCount >= 3) {
          console.warn("Ping timed out. Server is not responsive.");
          if (this.websocket) // Always reachable
            this.websocket.close(); // force reconnect
        }
        this.debug && console.log("ping");
        await this.ping();
        this.debug && console.log("pong");
        timeoutCount = 0;
      }, 5000);
    };

    this.websocket.onmessage = async (message: MessageEvent) => {
      if (message.data instanceof Blob) {
        const readerT = new FileReader();
        readerT.onloadend = async () => {
          await this.resolveRequest(readerT.result);
        };
        readerT.readAsText(message.data);
      } else {
        const u8arr = new Uint8Array(message.data);
        const str: string = R.reduce((str, byte) => str + String.fromCharCode(byte), "", Array.from(u8arr));
        await this.resolveRequest(str);
      }
    };

    this.debug && console.log("Waiting for server response...");
    const serverChallenge = await this.requests[-1].received;
    try {
      const result = await this.coder.answer(serverChallenge);
      const response = [result.iv,
                        result.encrypted,
                        result.authTag,
                        result.nonce];

      this.requests[-2].message = {
        id: -2,
        type: "clientAuth",
        response: response
      };

      this.debug && console.log("Authenticating websocket...");

      // Authentication should race against websocket.onclose
      await this.sendRequest(this.requests[-2]);

      this.invoke_cb("connected");
    } catch (err) {
      if (err instanceof E.RequestError) {
        throw new E.LoginRequired();
      } else {
        throw err;
      }
    }

    return;
  }

  private resolveRequest(responseText: string): void {
    const response = <Response>(JSON.parse(responseText));
    // Assume the response holds the message and response.id holds the
    //  message identifier that corresponds with it
    // response.result will hold the result if the API call succeeded,
    //  error message otherwise.
    const request = this.requests[response.id];
    const time = new Date();
    if (request.type !== "ping") {
      const diff = request.time ? time.getTime() - request.time : -1;
      if (response.success) {
        if (request.message.type !== "ping") {
          this.debug && console.log(`Request ${response.id} succeeded after ${diff} ms`, response);
        }
      } else {
        this.debug && console.warn(`Request ${response.id} failed after ${diff} ms`, request.message, response);
      }
    }

    if (response.id >= 0) {
      delete this.requests[response.id];
    } else if (this.requests[response.id].callback) {
      this.requests[response.id].callback(response.result);
    }
    if (response.success) {
      request.resolve(response.result);
    } else if (! this.isConnected()) {
      // test if is not authenticated
      // better have the backend reject with "unauthenticated" error,
      // instead of the current "invalid message"
      request.reject(new E.LoginRequired());
    } else {
      const diff = request.time ? time.getTime() - request.time : -1;
      request.reject(new E.RequestError(`Request ${request.message.id} failed with response: ${response.result}`,
                                        request,
                                        response));
    }
  }

  public disconnect(): void {
    if (this.websocket) {
      this.websocket.onclose = () => {};
      this.websocket.close();
    }
    this.websocket = undefined;
    this.connection = undefined;
  }

  public register_callback(type: string, cb: (message?: any) => any, now?: boolean): void {
    this.callbacks.push(new Callback(type, cb, now || false));

    if (type === "disconnected" && ! this.isConnected() && now) {
      cb();
    } else if (type === "connected" && this.isConnected() && now) {
      cb();
    } else if (type === "failed" && this.failed && now) {
      cb();
    }
  }

  public async invoke_cb(type: string, message?: any): Promise<Array<any>> {
    return this.callbacks.filter(
      (x: Callback) => { return x && x.type === type; }).map(
        async (x: Callback) => { return x.cb(message); });
  }

  // Helper function to invoke the I/O callback.
  private io_cb = () => {
    return async (message: any) => {
      return this.invoke_cb("io", message);
    };
  }

  private test_cb = () => {
    return async (message: any) => {
      return this.invoke_cb("test", message);
    };
  }

  private sendRequest<T>(request: Request<T>): Promise<T> {
    if (! this.isConnected()) {
      throw new E.NoInternet();
    }
    const msg   = request.message;
    const msgID = msg.id;
    this.requests[msgID] = request;
    const blob = JSON.stringify(msg);
    if (msg.type !== "ping") {
      this.debug && console.log(`Request ${msgID} was sent`, msg);
    }
    if (this.websocket) {
      this.websocket.send(blob);
    } else {
      throw new E.NoInternet();
    }
    return request.received;
  }

  /** Sends a message along the connection, ensuring that
   *  the server and client are properly authenticated.
   *
   *  If the socket has not been properly authenticated,
   *  sends the message after the socket has been properly
   *  authenticated/set up. */
  public sendMessage<T>(message: Message): Promise<T> {
    const msgID = this.lastMsgID++;
    message.id = msgID;
    return this.sendRequest<T>(new Request<T>(message));
  }

  public isConnected() {
    return this.websocket &&
           this.websocket.readyState === this.websocket.OPEN;
  }

  public async ping(): Promise<void> {
    await this.sendMessage({
      type: "ping"
    });
  }

  public getUsername(): string {
    if (!this.connection) {
      throw new E.WebsocketError("Trying to access username when the connection is not set.");
    }
    return this.connection.username;
  }

}
