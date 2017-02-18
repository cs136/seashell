export class SeashellWebsocket {
  private synced: boolean;
  private connected: boolean;
  private failed: boolean;
  private isSyncing: boolean;
  private offlineMode: number;
  private timeoutCount: number;
  private timeoutInterval: any;
  private key: number;

  class Callback {
    public type: string;
    public cb: (message?: any) => any;
    public now: boolean;
  }
  private callbacks: Callback[];

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

  public register_callback(type: string, cb: (message?: any) => Any, now?: boolean) : number {
    this.callbacks[this.key] = new Callback(type, cb, now);

    if(type === 'disconnected' && !this.connected && now) {
      cb();
    } else if(type === 'connected' && (this.connected || this.isOffline()) && now) {
      cb();
    } else if(type === 'failed' && this.failed && now) {
      cb();
    }
    return key++;
  }

  public unregister_callback(key: number) : void {
    delete this.callbacks[key];
  }

  public unregister_callbacks(type: string) : void {
    this.callbacks = _.filter(this.callbacks,
      (item: Callback) : boolean => return item && item.type===type);
    this.key = this.callbacks.length;
  }

  public invoke_cb(type: string, message?: any): PromiseLike<Array<any>> {
    return _.map(_.filter(this.callbacks,
      (x: Callback)=>return x && x.type === type),
      (x: Callback)=>return x.cb(message));
  }

  // wrapper function for invoke_cb meant to be used to call
  //  the websocket failure coniditons 'disconnected' and 'failure'
  public invoke_cb_failure_om_wrap(type: string, message?: any): PromiseLike<Array<any>> {
    if(this.offlineEnabled()) {
      // if offline mode is enabled, we notify the frontend
      //  and proceed as if we are connected
      return this.invoke_cb('connected', this.offline_mode);
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
}
