import {History,Settings,Change} from './types';
import WebSocket = require("ws");
import {Coder, AuthKey} from './crypto';
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


class SeashellWebsocket {
  private coder: Coder;
  private lastRequest: number;
  public requests: {[index:number]: Request<any>};
  // public ready: Promise<boolean>;
  private authenticated: boolean;
  private server_nonce: AuthKey;
  private failed: boolean;
  private closed: boolean;
  private started: boolean;
  private websocket: WebSocket;
  private closes: () => void;
  private falures: () => void;

  // this allows WebsocketService to access member functions by string key
  [key: string]: any;

  constructor(private uri: string, 
              private key: AuthKey) {
    this.coder = new Coder(key);
    console.warn(`Using key ${key} to create Coder.`);
    this.websocket = new WebSocket(this.uri);
    this.lastMsgID = 0;
    this.authenticated = false;
    this.failed = false;
    this.closed = false;
    this.started = false;
    this.requests = {};
    this.requests[-1] = new Request({id: -1}); // server challenge
    this.requests[-2] = new Request({id: -2}); // reply challenge
    this.requests[-3] = new Request({id: -3});
    this.requests[-4] = new Request({id: -4});
    // this.ready = new Promise((resolve, reject)=>{
    this.started = true;
    this.websocket.onerror = () => {
      this.failed = true;
      if (! this.authenticated) {
        throw new WebsocketError("Socket closed during authentication!");
      }
      return this.closes && this.closes();
    };

    this.websocket.onclose = () => {
      this.closed = true;
      if (! this.authenticated) {
        throw new WebsocketError("Socket closed during authentication!");
      }
      return this.closes && this.closes();
    };

    this.websocket.onmessage = (message) => {
      try {
        const response_string = String.fromCharCode.apply(null, new Uint32Array(message.data));
        var response = <Response>(JSON.parse(response_string));
        // Assume the response holds the message and response.id holds the
        //  message identifier that corresponds with it
        //
        // response.result will hold the result if the API call succeeded,
        //  error message otherwise.
        const request = this.requests[response.id];

        if (request.type != 'ping') {
          const time = new Date();
          const diff = request.time ? time.getTime() - request.time : -1;
          if (response.success) {
            console.log(`Request ${response.id} succeeded after ${diff} ms. \n`, response);
          } else {
            console.warn(`Request ${response.id} failed after ${diff} ms.\n`, response);
          }

        }

        if (response.success) {
          request.resolve(response.result);
        } else if (! response.success) {
          request.reject(`Request ${request.message.id} failed with response: ${response.result}`);
        }

        if (response.id >= 0) {
          delete this.requests[response.id];
        }
      } catch (err) {
        console.error("websocket.onmessage:", err);
        console.error("websocket.onmessage received:", response);
      }
    }
  }

  public onFailure(callbacks: () => void) {
    this.failures = callbacks;
  }

  public onClose(callbacks: () => void) {
    this.closes = callbacks;
  }

  public close(): void {
    this.websocket.close();
  }

  // public async answerChallenge(serverChallenge: Uint8Array): Promise<{}>

  public async authenticate(): Promise<void> {
    const server_challenge = new Uint8Array(await this.requests[-1].received);

    const result = await this.coder.answer(server_challenge);
    const response = [Array.from(result.iv), 
                      Array.from(result.encrypted), 
                      Array.from(result.authTag), 
                      Array.from(result.nonce)];

/*
    // // Generate a nonce
    server_challenge = new Uint32Array(server_challenge);
    let client_nonce = new Uint32Array(crypto.randomBytes(128));
    // for (let i = 0; i < client_nonce.length; i++) {
    //   client_nonce[i] = client_nonce[i] & 0xFF;
    // }
    // // OK, now we proceed to authenticate.
    let raw_response = new Uint32Array(Array.from(client_nonce).concat(Array.from(server_challenge)));
    // raw_response.set(client_nonce, 0);
    // raw_response.set(server_challenge, client_nonce.byteLength);
    const iv  = new Uint8Array(crypto.randomBytes(48).buffer);
    const key = new Uint32Array(this.key);
    const authdata = iv;
    // console.warn(this.key, key.byteLength, iv.byteLength, raw_response.byteLength);
    let encrypted = aesgcm.encrypt(Buffer.from(key.buffer), 
                                    Buffer.from(iv.buffer), 
                                    Buffer.from(raw_response.buffer), 
                                    Buffer.from(authdata.buffer));
    const ciphertext = new Uint8Array(encrypted.ciphertext);
    const auth_tag   = new Uint8Array(encrypted.auth_tag);
    let response = [Array.from(iv), Array.from(ciphertext), Array.from(auth_tag), Array.from(client_nonce)];
//*/
    this.requests[-2].message = {
      id: -2,
      type: 'clientAuth', 
      response: response
    };
    this.sendRequest(this.requests[-2]);
    console.warn(`Answer with int[${response[0].length}], int[${response[1].length}], int[${response[2].length}], int[${response[3].length}]`);
    try {
      const result = await this.requests[-2].received;
      console.log("Authentication success");
      this.authenticated = true;
    } catch (err) {
      throw new WebsocketError(`Authentication failure: ${err.msg}`);
    }
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
      console.log(`Request ${msgID} was sent.\n`, displayMsg);
      // console.log(blob);
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
  private sendMessage(message: Message): Promise<WebsocketResult> {
    if (this.failed || this.closed || ! this.started) {
      throw new WebsocketError("Socket closed or failed");
    }
    var msgID = this.lastMsgID++;
    message.id = msgID;
    return this.sendRequest(new Request(message));
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

  public async compileAndRunProject(project: string, question: string, test: Array<string>): Promise<WebsocketResult> {
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

  public async getProjects(): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'getProjects'});
  }

  public async listProject(name: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'listProject',
      project: name});
  }

  public async newProject(name: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'newProject',
      project: name});
  }

  public async newProjectFrom(name: string, src_url: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'newProjectFrom',
      project: name,
      source: src_url});
  }

  public async deleteProject(name: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'deleteProject',
      project: name});
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

  public async readFile(name: string, fileName: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'readFile',
      project: name,
      file: fileName});
  }

  public async newFile(name: string, fileName: string, contents: string, encoding: string, normalize: boolean): Promise<WebsocketResult> {
    if (contents) {
      return await this.sendMessage({
        type: 'newFile',
        project: name,
        file: fileName,
        contents: contents,
        encoding: encoding || 'raw',
        normalize: normalize});
    } else {
      return await this.sendMessage({
        type: 'newFile',
        project: name,
        file: fileName,
        normalize: false});
    }
  }

  public async restoreFileFrom(name: string, file: string, url: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'restoreFileFrom',
      project: name,
      file: file,
      template: url});
  }

  public async writeFile(name: string, file: string, contents: string, history: History): Promise<WebsocketResult> {
    if (history) {
      return await this.sendMessage({
        type: 'writeFile',
        project: name,
        file: file,
        contents: contents,
        history: history});
    } else {
      return await this.sendMessage({
        type: 'writeFile',
        project: name,
        contents: contents,
        history: false});
    }
  }

  public async deleteFile(name: string, file: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'deleteFile',
      project: name,
      file: file});
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

  public async renameFile(name: string, oldName: string, newName: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'renameFile',
      project: name,
      oldName: oldName,
      newName: newName});
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

  public async saveSettings(settings: Settings): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'saveSettings',
      settings: settings});
  }

  public async getSettings(): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'getSettings'});
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

  public async getFileToRun(name: string, question: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'getFileToRun',
      project: name,
      question: question});
  }

  public async setFileToRun(name: string, question: string, folder: string, file: string): Promise<WebsocketResult> {
    return await this.sendMessage({
      type: 'setFileToRun',
      project: name,
      question: question,
      folder: folder,
      file: file});
  }

  public async sync(message: Message) {
    message.type = 'sync';
    return await this.sendMessage(message);
  }
}
