import {SeashellWebsocket} from './websocket_client';

class Callback {
  constructor(public type: string, public cb: (message?: any) => any, public now: boolean) { }
}

export class WebsocketService {
  private synced: boolean;
  private connected: boolean;
  private failed: boolean;
  private isSyncing: boolean;
  private offlineMode: number;
  private timeoutCount: number;
  private timeoutInterval: any;
  private key: number;

  private callbacks: Callback[];
  private socket: SeashellWebsocket;

  constructor() {
    this.synced = false;
    this.connected = false;
    this.failed = false;
    this.isSyncing = false;
    this.offlineMode = 0; // TODO change this to look up the cookie
    this.timeoutCount = 0;
    this.timeoutInterval = null;
    this.key = 0;
  }

  public register_callback(type: string, cb: (message?: any) => any, now?: boolean) : number {
    this.callbacks[this.key] = new Callback(type, cb, now);

    if(type === 'disconnected' && !this.connected && now) {
      cb();
    } else if(type === 'connected' && (this.connected || this.isOffline()) && now) {
      cb();
    } else if(type === 'failed' && this.failed && now) {
      cb();
    }
    return this.key++;
  }

  public unregister_callback(key: number) : void {
    delete this.callbacks[key];
  }

  public unregister_callbacks(type: string) : void {
    this.callbacks = this.callbacks.filter(
      (item: Callback) : boolean => { return item && item.type===type; });
    this.key = this.callbacks.length;
  }

  public async invoke_cb(type: string, message?: any): Promise<Array<any>> {
    return this.callbacks.filter(
      (x: Callback)=>{ return x && x.type === type; }).map(
        async (x: Callback)=>{ return await x.cb(message); });
  }

  // wrapper function for invoke_cb meant to be used to call
  //  the websocket failure coniditons 'disconnected' and 'failure'
  public invoke_cb_failure_om_wrap(type: string, message?: any): PromiseLike<Array<any>> {
    if(this.offlineEnabled()) {
      // if offline mode is enabled, we notify the frontend
      //  and proceed as if we are connected
      return this.invoke_cb('connected', this.offlineMode);
    }
    return this.invoke_cb(type, message);
  }

  // Helper function to invoke the I/O callback.
  public io_cb(ignored: any, message: any) {
    return this.invoke_cb('io', message);
  }

  public test_cb(ignored: any, message: any) {
    return this.invoke_cb('test', message);
  }

  // Connects the socket, sets up the disconnection monitor
  public async connect(): Promise<boolean> {
    /* TODO get cookie here
    if(! get(SEASHELL_CREDS_COOKIE)) {
      this.failed = true;
      this.invoke_cb('failed');
      throw error;
    }*/
    try {
      /* TODO need cookie here
      this.socket = new SeashellWebsocket(sprintf("wss://%s:%d", SEASHELL_CREDS_COOKIE.host, SEASHELL_CREDS_COOKIE.port), SEASHELL_CREDS_COOKIE.key,
      // Failure - probably want to prompt the user to attempt to reconnect or
      //  log in again
      ()=>{
        this.failed = true;
        clearInterval(this.timeoutInterval);
        this.invoke_cb_failure_om_wrap('failed'); },
      // Socket closed - probably want to prompt the user to reconnect
      ()=>{
        this.connected = false;
        clearInterval(this.timeoutInterval);
        self.invoke_cb_failure_om_wrap('disconnected'); });
        */
    } catch(e) {
      this.failed = true;
      this.invoke_cb_failure_om_wrap('failed');
      throw e;
    }
    await this.socket.ready;
    console.log("Seashell socket set up properly");
    this.timeoutInterval = setInterval(async ()=>{
      if(this.timeoutCount++ === 3) {
        this.invoke_cb('timeout');
      }
      await this.socket.ping();
      if(this.timeoutCount >= 3) {
        this.invoke_cb('timein');
      }
      this.timeoutCount = 0;
    }, 4000);
    this.connected = true;
    this.failed = false;
    this.socket.requests[-3].callback = this.io_cb;
    this.socket.requests[-4].callback = this.test_cb;
    console.log("Websocket disconnection monitor set up properly.");
    // Run the callbacks.
    this.invoke_cb('connected');
    return true;
  }

  public isOffline(): boolean {
    return false;
  }

  public offlineEnabled(): boolean {
    return false;
  }
}
