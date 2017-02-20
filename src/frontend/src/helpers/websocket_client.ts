import {Coder} from './crypto';
import * as sjcl from 'sjcl';

class Message {
  type: string;
  project?: string;
  question?: string;
  pid?: number;
  file?: string;
  tests?: Array<string>;
  source?: string;
  contents?: string;
  encoding?: string;
  normalize?: boolean;
  template?: string;
  history?: History | boolean;
  oldName?: string;
  newName?: string;
  response?: ArrayBuffer[];
  constructor(msg: Message) {
    this.type = msg.type;
    this.project = msg.project;
    this.question = msg.question;
    this.pid = msg.pid;
    this.file = msg.file;
    this.tests = msg.tests;
    this.response = msg.response;
  }
}

// TODO flesh out the History type for file histories
interface History { }

class Request extends Message {
  callback: (success: boolean, result: WebsocketResult)=>void;
  time: number;
  id?: number;
  resolve?: (item:any)=>void;
  reject?: (item:any)=>void;
  constructor(msg?: Message) {
    super(msg);
  }
}

export class WebsocketResult extends Message { }

class AuthResult extends WebsocketResult implements ArrayBuffer {
  byteLength: number;
  slice: any;
}

class Response {
  id: number;
  success: boolean;
  result: WebsocketResult;
}

export class SeashellWebsocket {
  private coder: Coder;
  private lastRequest: number;
  public requests: Request[];
  public ready: Promise<boolean>;
  private authenticated: boolean;
  private server_nonce: Uint8Array;
  private failed: boolean;
  private closed: boolean;
  private started: boolean;
  private websocket: WebSocket;

  constructor(private uri: string, private key: Uint8Array, private failure: ()=>void, private closes: ()=>void) {
    this.coder = new Coder(this.key);
    this.lastRequest = 0;
    this.authenticated = false;
    this.failed = false;
    this.closed = false;
    this.started = false;

    this.requests[-1] = {
      callback: this.authenticate, type: null, time: null
    };
    this.requests[-3] = new Request();
    this.requests[-4] = new Request();

    this.ready = new Promise((resolve, reject)=>{
      this.websocket = new WebSocket(this.uri);
      this.started = true;
      this.websocket.onerror = ()=>{
        this.failed = true;
        if(!this.authenticated) {
          reject("Socket closed during authentication!");
        }
        return this.closes && this.closes();
      };
      this.websocket.onclose = ()=>{
        this.closed = true;
        if(!this.authenticated) {
          reject("Socket closed during authentication!");
        }
        return this.closes && this.closes();
      };

      this.websocket.onmessage = (message: MessageEvent)=> {
        var readerT = new FileReader();
        readerT.onloadend = ()=>{
          var response_string = readerT.result;
          var response = <Response>(JSON.parse(response_string));
          // Assume the response holds the message and response.id holds the
          //  message identifier that corresponds with it
          //
          // response.result will hold the result if the API call succeeded,
          //  error message otherwise.
          var request = this.requests[response.id];

          if(request.type != 'ping') {
            var time = new Date();
            var diff = request.time ? time.getTime() - request.time : -1;
            console.log("Received response to message with id "+response.id+" after "+diff+" ms.");

            if(!response.success) {
              console.error(response);
            } else {
              console.log(response);
            }
          }

          if(response.success && request.resolve) {
            request.resolve(response.result);
          } else if(!response.success && request.reject) {
            request.reject(response.result);
          }

          if(request.callback) {
            request.callback(response.success, response.result);
          }
          if(response.id >= 0) {
            delete this.requests[response.id];
          }
        };
        readerT.readAsText(message.data);
      };
      resolve(true);
    });
  }

  public close(): void {
    this.websocket.close();
  }

  private async authenticate(success: boolean, server_challenge: AuthResult): Promise<boolean> {
    // Generate a nonce
    var client_nonce = new Uint8Array(sjcl.random.randomWords(32));
    for(var i=0; i<client_nonce.length; i++) {
      client_nonce[i] = client_nonce[i] & 0xFF;
    }

    // OK, now we proceed to authenticate.
    var raw_response = new Uint8Array(client_nonce.byteLength + server_challenge.byteLength);
    raw_response.set(new Uint8Array(client_nonce), 0);
    raw_response.set(new Uint8Array(server_challenge), client_nonce.byteLength);
    var encrypted = await this.coder.encrypt(raw_response);
    var response = [encrypted.iv, encrypted.encrypted, encrypted.tag, client_nonce];

    try {
      var result = await this.sendMessage({type: 'clientAuth', response: response});
      console.log("Authenticated!");
      this.authenticated = true;
      return true;
    } catch(e) {
      return false;
    }
  }

  /** Sends a message along the connection. Internal use only.
   *
   * @param {Object} message - JSON message to send (as JavaScript object).
   * @returns {Promise} */
  private _sendMessage(message: Request): Promise<WebsocketResult> {
    var request_id = this.lastRequest++;

    message.time = (new Date()).getTime();

    this.requests[request_id] = message;
    message.id = request_id;
    // Stringify, write out as Array of bytes
    var blob = new Blob([JSON.stringify(message)]);

    // log to console
    if(message.type !== 'ping') {
      var displayMsg = Object.assign({}, message);
      delete displayMsg.time;
      console.log("Sent request with id "+request_id+".");
      console.log(displayMsg);
    }
    
    return new Promise((resolve, reject)=>{
      try {
        this.websocket.send(blob);
        this.requests[request_id].resolve = resolve;
        this.requests[request_id].reject = reject;
      } catch(e) {
        reject(e);
      }
    });
  }

  /** Sends a message along the connection, ensuring that
   *  the server and client are properly authenticated.
   *
   *  If the socket has not been properly authenticated,
   *  sends the message after the socket has been properly
   *  authenticated/set up. */
  private async sendMessage(message: Message): Promise<WebsocketResult> {
    if(this.failed || this.closed || !this.started) {
      throw "Socket closed or failed!";
    }
    else if(this.authenticated) {
      return await this._sendMessage(new Request(message));
    }
    else {
      await this.ready;
      return await this._sendMessage(new Request(message));
    }
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
    if(contents) {
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
    if(history) {
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

  public async getMostRecentlyUsed(name: string, question: string) {
    return await this.sendMessage({
      type: 'getMostRecentlyUsed',
      project: name,
      question: question});
  }

  public async updateMostRecentlyUsed(name: string, question: string, file: string) {
    return await this.sendMessage({
      type: 'updateMostRecentlyUsed',
      project: name,
      question: question,
      file: file});
  }
}
