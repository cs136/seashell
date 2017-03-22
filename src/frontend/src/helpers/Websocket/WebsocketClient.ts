//import WebSocket = require("ws");
import {Coder, ShittyCoder} from './Crypto';
import {Connection} from '../Services';
import WebCrypto = require("node-webcrypto-ossl");

export {WebsocketResult, SeashellWebsocket, WebsocketError};

class WebsocketError extends Error {
  data: Object;
  constructor(msg: string, d?: Object) {
    super(msg);
    this.data = d;
  }
}

interface Message {
  [index: string]: any;
  // type: string;
  // project?: string;
  // question?: string;
  // folder?: string;
  // pid?: number;
  // file?: string;
  // tests?: Array<string>;
  // source?: string;
  // contents?: string;
  // encoding?: string;
  // normalize?: boolean;
  // template?: string;
  // history?: History | false;
  // oldName?: string;
  // newName?: string;
  // settings?: Settings;
  // assn?: string;
  // subdir?: string | false;
  // location?: string | false;
  // response?: ArrayBuffer[];
  // projects?: Array<string>;
  // files?: Array<string>;
  // changes?: Array<Change>;
  // type     = msg && msg.type;
  // project  = msg && msg.project;
  // question = msg && msg.question;
  // pid      = msg && msg.pid;
  // file     = msg && msg.file;
  // tests    = msg && msg.tests;
  // response = msg && msg.response;
}

class Request<T> {
  [index: string]: any;
  public time: number;
  public received: Promise<T>; // resolves when the response message is received
  public resolve: (value: T) => any | PromiseLike<any>;
  public reject: (reason: any) => any | PromiseLike<any>
  constructor(public message: Message) {
    this.time = Date.now();
    this.received = new Promise<any>((s,f)=>{
      this.resolve = s;
      this.reject  = f;
    });
  }
}

interface WebsocketResult extends Message {
  newProjects: Array<string>;
  deletedProjects: Array<string>;
  updatedProjects: Array<string>;
}

class Response {
  id: number;
  success: boolean;
  result: WebsocketResult;
}

class Callback {
  constructor(public type: string, public cb: (message?: any) => any, public now: boolean) { }
}

class SeashellWebsocket {
  private cnn: Connection;
  private coder: ShittyCoder;
  private websocket: WebSocket;
  private lastMsgID: number;
  public requests: {[index:number]: Request<any>};
  private authenticated: boolean;
  private failed: boolean;
  private closed: boolean;
  private started: boolean;
  private closes: () => void;
  private failures: () => void;
  public debug: boolean; // toggle console.log for tests

  private timeoutCount: number;
  private timeoutInterval: any;
  private key: number;
  private callbacks: Callback[];

  // this allows WebsocketService to access member functions by string key
  //[key: string]: any;

  constructor(debug?: boolean) {
    this.lastMsgID = 0;
    this.authenticated = false;
    this.failed = false;
    this.closed = false;
    this.started = false;
    this.debug = !!debug;
    this.requests = {};
    this.requests[-1] = new Request({id: -1}); // server challenge
    this.requests[-2] = new Request({id: -2}); // reply challenge
    this.requests[-3] = new Request({id: -3});
    this.requests[-4] = new Request({id: -4});
    // this.ready = new Promise((resolve, reject)=>{
    this.started = true;
    this.timeoutCount = 0;
    this.timeoutInterval = null;
    this.callbacks = [];
  }

  // public async answerChallenge(serverChallenge: Uint8Array): Promise<{}>

  // Connects and authenticates the socket, sets up the disconnection monitor
  // Pass a new Connection object to overwrite the previously held one
  public async connect(cnn?: Connection): Promise<void> {
    if(cnn) {
      this.cnn = cnn;
    }
    else if(!this.cnn) {
      throw new WebsocketError("Trying to connect websocket with no Connection object provided.");
    }
    this.coder = new ShittyCoder(this.cnn.key);
    this.websocket = new WebSocket(this.cnn.wsURI);
    this.websocket.onerror = () => {
      this.failed = true;
      if(!this.authenticated) {
        throw new WebsocketError("Socket closed during authentication!");
      }
      return this.closes && this.closes();
    };

    this.websocket.onclose = () => {
      this.closed = true;
      if(!this.authenticated) {
        throw new WebsocketError("Socket closed during authentication!");
      }
      return this.closes && this.closes();
    };

    this.websocket.onmessage = (message: any) => {
      try {
        const response_string = String.fromCharCode.apply(null, new Uint32Array(message.data));
        var response = <Response>(JSON.parse(response_string));
        // Assume the response holds the message and response.id holds the
        //  message identifier that corresponds with it
        // response.result will hold the result if the API call succeeded,
        //  error message otherwise.
        const request = this.requests[response.id];

        if(request.type != 'ping') {
          const time = new Date();
          const diff = request.time ? time.getTime() - request.time : -1;
          if (response.success) {
            this.debug && console.log(`Request ${response.id} succeeded after ${diff} ms. \n`, response);
          } else {
            console.warn(`Request ${response.id} failed after ${diff} ms.\n`, request.message, response);
          }

        }

        if(response.success) {
          request.resolve(response.result);
        } else if(!response.success) {
          request.reject(`Request ${request.message.id} failed with response: ${response.result}`);
        }

        if(response.id >= 0) {
          delete this.requests[response.id];
        }
      } catch(err) {
        console.error("websocket.onmessage:", err);
        console.error("websocket.onmessage received:", response);
      }
    }

    const server_challenge = await this.requests[-1].received;

    const result = await this.coder.answer(server_challenge);
    const response = [result.iv,
                      result.encrypted,
                      result.authTag,
                      result.nonce];

    this.requests[-2].message = {
      id: -2,
      type: 'clientAuth',
      response: response
    };
    this.sendRequest(this.requests[-2]);
    try {
      const result = await this.requests[-2].received;
      this.debug && console.log("Authentication success");
      this.authenticated = true;
    } catch(err) {
      throw new WebsocketError(`Authentication failure: ${err.msg}`);
    }

    // Failure - probably want to prompt the user to attempt to reconnect or
    //  log in again
    this.failures = async () => {
      clearInterval(this.timeoutInterval);
      try {
        await this.invoke_cb('failed');
      } catch(err) {
        console.error(err);
      }
    };

    // Socket closed - probably want to prompt the user to reconnect
    this.closes = async () => {
      clearInterval(this.timeoutInterval);
      try {
        await this.invoke_cb('disconnected');
      } catch (err) {
        console.error(err);
      }
    };

    if(!this.authenticated) {
      await this.invoke_cb('failed');
      throw new WebsocketError("socket is not authenticated");
    }

    this.debug && console.log("Seashell socket set up properly");
    this.timeoutInterval = setInterval(async () => {
      try {
        if (this.timeoutCount++ === 3) {
          this.invoke_cb('timeout');
        }
        await this.ping();
        if (this.timeoutCount >= 3) {
          this.invoke_cb('timein');
        }
        this.timeoutCount = 0;
      } catch (err) {
        console.error(err);
      }
    }, 4000);
    this.requests[-3].callback = this.io_cb;
    this.requests[-4].callback = this.test_cb;
    this.debug && console.log("Websocket disconnection monitor set up properly.");
    // Run the callbacks.
    await this.invoke_cb('connected');
  }

  public disconnect(): void {
    this.websocket.close();
  }

   public register_callback(type: string, cb: (message?: any) => any, now?: boolean) : number {
    this.callbacks[this.key] = new Callback(type, cb, now);

    if(type === 'disconnected' && !this.isConnected() && now) {
      cb();
    } else if(type === 'connected' && this.isConnected() && now) {
      cb();
    } else if(type === 'failed' && this.failed && now) {
      cb();
    }
    return this.key++;
  }

  public unregister_callback(key: number) : void {
    delete this.callbacks[key];
  }

  public async invoke_cb(type: string, message?: any): Promise<Array<any>> {
    return this.callbacks.filter(
      (x: Callback) => { return x && x.type === type; }).map(
        async (x: Callback) => { return x.cb(message); });
  }

  // Helper function to invoke the I/O callback.
  public io_cb(ignored: any, message: any) {
    return this.invoke_cb('io', message);
  }

  public test_cb(ignored: any, message: any) {
    return this.invoke_cb('test', message);
  } 

  /** Sends a message along the connection. Internal use only.
   *
   * @param {Object} message - JSON message to send (as JavaScript object).
   * @returns {Promise} */
  private sendRequest(request: Request<WebsocketResult>): Promise<WebsocketResult> {
    // message.time = (new Date()).getTime();
    const msg   = request.message;
    const msgID = msg.id;
    this.requests[msgID] = request;
    // Stringify, write out as Array of bytes
    var blob = JSON.stringify(msg);

    // log to console
    if (msg.type !== 'ping') {
      var displayMsg = Object.assign({}, msg);
      delete displayMsg.time;
      this.debug && console.log(`Request ${msgID} was sent.\n`, displayMsg);
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
  public sendMessage(message: Message): Promise<any> {
    if(!this.isConnected()) {
      throw new WebsocketError("Socket closed or failed");
    }
    var msgID = this.lastMsgID++;
    message.id = msgID;
    return this.sendRequest(new Request(message));
  }

  public isConnected() {
    return !(this.failed || this.closed || !this.started);
  }

  /** The following functions are wrappers around sendMessage.
   *  Consult dispatch.rkt for a full list of functions.
   *  These functions take in arguments as specified in server.rkt
   *  and return a JQuery Deferred object. */

  public async ping(): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'ping'
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
