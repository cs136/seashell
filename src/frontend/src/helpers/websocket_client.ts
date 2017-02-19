import {Coder} from './crypto';
import * as sjcl from 'sjcl';

interface Request {
  callback: (type: string, result:any)=>any;
  type: string;
  time: number;
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
    this.requests[-3] = {
      callback: null, type: null, time: null
    };
    this.requests[-4] = {
      callback: null, type: null, time: null
    };

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

      this.websocket.onmessage = (message:any)=> {
        var readerT = new FileReader();
        readerT.onloadend = ()=>{
          var response_string = readerT.result;
          var response = JSON.parse(response_string);
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

  private async authenticate(ign: any, server_challenge: Uint8Array): Promise<boolean> {
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

    var result = await this.sendMessage({type: 'clientAuth', response: response});
    console.log("Authenticated!");
    this.authenticated = true;
    return true;
  }

  private async sendMessage(message: any) {

  }

  public async ping() { }
}
