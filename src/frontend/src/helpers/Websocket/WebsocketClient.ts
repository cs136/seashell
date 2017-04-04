import {Coder, ShittyCoder} from "./Crypto";
import {Connection} from "../Services";
import WebCrypto = require("node-webcrypto-ossl");
import * as R from "ramda";
import * as E from "../Errors";
import {Message, Request, Response, WebsocketResult, Callback} from "./Interface";

export {SeashellWebsocket, MockInternet}

enum MockInternet {Online, Offline, Real};

class SeashellWebsocket {
  private connection: Connection;
  private coder: ShittyCoder;
  private websocket: WebSocket;
  private lastMsgID: number;
  public requests: {[index: number]: Request<any>};
  private authenticated: boolean;
  private closes: () => void;
  private failures: () => void;
  public debug: boolean; // toggle console.log for tests
  private pingLoop: any;
  private callbacks: Callback[];
  public mockInternet: MockInternet;
  // this allows WebsocketService to access member functions by string key
  // [key: string]: any;

  constructor(debug?: boolean) {
    this.debug = debug;
    this.callbacks = [];
  }

  // Connects and authenticates the socket, sets up the disconnection monitor
  // Pass a new Connection object to overwrite the previously held one
  // It must be safe to call this function consecutively many times
  public async connect(cnn: Connection): Promise<void> {
    this.debug && console.log("Connecting to websocket...");
    this.lastMsgID = 0;
    this.authenticated = false;
    this.requests = {};
    this.requests[-1] = new Request({id: -1}); // server challenge
    this.requests[-2] = new Request({id: -2}); // reply challenge
    this.requests[-3] = new Request({id: -3});
    this.requests[-4] = new Request({id: -4});
    this.requests[-3].callback = this.io_cb;
    this.requests[-4].callback = this.test_cb;
     // return resolved promise at the end
    let rtvResolve: () => void;
    let rtvReject: (reason: any) => void;
    let rtv: Promise<void> = new Promise<void>((resolve, reject) => {
      rtvResolve = resolve;
      rtvReject = reject;
    });
    // if there's an exisitng websocket,
    // if it's connection or open: do nothing
    // if it's closing or closed: schedule to open a new connection
    if (this.websocket) {
      switch (this.websocket.readyState) {
        case this.websocket.CONNECTING: {
          this.debug && console.log("Socket is already connecting. Action ignored.");
          return;
        }
        case this.websocket.OPEN: {
          this.debug && console.log("Socket is already connected. Action ignored.");
          return;
        }
        case this.websocket.CLOSING: {
          this.debug && console.log(`Existing websocket is closing. Wait to reopen new connection.`);
          const promise = new Promise<void>((accept, reject) => {
            this.websocket.onclose = () => {
              this.connect(cnn).then(accept);
            };
          });
          return promise;
        }
        case this.websocket.CLOSED: {
          this.debug && console.log(`Existing websocket is closed. Reopening new connection.`);
        }
      }
    }

    this.connection = cnn;
    this.coder = new ShittyCoder(this.connection.key);

    try {
      this.websocket = new WebSocket(this.connection.wsURI);
    } catch (err) {
      console.error(`Could not create WebSocket connection to ${this.connection.wsURI}:\n${err}`);
      throw new E.LoginRequired(); // simply ask user to retry
    }

    // Websocket.onclose should race against authentication
    this.websocket.onclose = () => {
      rtvReject(new E.NoInternet());
      console.warn(`Websocket lost connection. Trying to reconnect...`);
      clearInterval(this.pingLoop);
      // automatically reconnect after 3s
      if (this.connection) {
        setTimeout(() => {
          this.connect(this.connection);
        }, 3000);
      }
      for (const i in this.requests) {
        this.requests[i].reject(new E.NoInternet());
      }
    };

    this.websocket.onerror = (err) => {
      console.error(`Websocket encountered error. Closing websocket.\n${err}`);
      this.websocket.close();
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
    this.sendRequest(this.requests[-2]).then(() => {
      this.debug && console.log("Authentication succeeded.");
      this.authenticated = true;
      this.debug && console.log("Seashell is ready :)");
      rtvResolve();
    }).catch((err) => {
      if (err instanceof E.RequestError) {
        rtvReject(new E.LoginRequired());
      }
      rtvReject(err);
    });

    return rtv;
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
    }
    if (response.success) {
      request.resolve(response.result);
    } else if (! this.authenticated) {
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
      this.websocket.close();
    }
    this.websocket = undefined;
    this.connection = undefined;
  }

  // maybe consider this.websocket.addEventListener?

  public register_callback(type: string, cb: (message?: any) => any, now?: boolean): void {
  // this.callbacks[this.key] = new Callback(type, cb, now);

  //   if (type === "disconnected" && !this.isConnected() && now) {
  //     cb();
  //   } else if (type === "connected" && this.isConnected() && now) {
  //     cb();
  //   } else if (type === "failed" && this.failed && now) {
  //     cb();
  //   }
  //   return this.key++;
  }

  public unregister_callback(key: number): void {
    delete this.callbacks[key];
  }

  public async invoke_cb(type: string, message?: any): Promise<Array<any>> {
    return this.callbacks.filter(
      (x: Callback) => { return x && x.type === type; }).map(
        async (x: Callback) => { return x.cb(message); });
  }

  // Helper function to invoke the I/O callback.
  public io_cb(ignored: any, message: any) {
    return this.invoke_cb("io", message);
  }

  public test_cb(ignored: any, message: any) {
    return this.invoke_cb("test", message);
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
    this.websocket.send(blob);
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
    return this.sendRequest(new Request(message));
  }

  public isConnected() {
    if (this.mockInternet === MockInternet.Offline) {
      return false;
    }
    if (this.mockInternet === MockInternet.Online) {
      return true;
    }
    return this.websocket &&
           this.websocket.readyState === this.websocket.OPEN;
  }

  public async ping(): Promise<void> {
    await this.sendMessage({
      type: "ping"
    });
  }

  /*public async compileAndRunProject(project: string, question: string, test: Array<string>): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'compileAndRunProject',
      project: project,
      question: question,
      tests: test});
  }

  public async programKill(pid: number): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'programKill',
      pid: pid});
  }

  public async sendEOF(pid: number): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'sendEOF',
      pid: pid});
  }

  public async newProjectFrom(name: string, src_url: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'newProjectFrom',
      project: name,
      source: src_url});
  }


  public async lockProject(name: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'lockProject',
      project: name});
  }

  public async forceLockProject(name: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'forceLockProject',
      project: name});
  }

  public async unlockProject(name: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'unlockProject',
      project: name});
  }

  public async restoreFileFrom(name: string, file: string, url: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'restoreFileFrom',
      project: name,
      file: file,
      template: url});
  }

  public async programInput(pid: number, contents: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'programInput',
      pid: pid,
      contents: contents});
  }

  public async getExportToken(name: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'getExportToken',
      project: name});
  }

  public async getUploadFileToken(name: string, file: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'getUploadFileToken',
      project: name,
      file: file});
  }

  public async getMostRecentlyUsed(name: string, question: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'getMostRecentlyUsed',
      project: name,
      question: question});
  }

  public async updateMostRecentlyUsed(name: string, question: string, file: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'updateMostRecentlyUsed',
      project: name,
      question: question,
      file: file});
  }

  public async marmosetSubmit(name: string, assn: string, subdir?: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'marmosetSubmit',
      project: name,
      assn: assn,
      subdir: subdir ? subdir : false});
  }

  public async startIO(name: string, pid: number): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'startIO',
      project: name,
      pid: pid});
  }

  public async archiveProjects(): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'archiveProjects',
      location: false});
  }

  public async sync(message: Message) {
    message.type = 'sync';
    return await this.sendMessage(message);
  }*/
}
