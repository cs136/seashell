import {SeashellWebsocket, WebsocketResult, WebsocketError} from "./websocket_client";
import {WSConnection} from "./login";
import {Storage} from "./storage";
import {History,Settings,Test,SeashellFile,SeashellCompiler,SeashellRunner,SeashellTester,SeashellPID,Change} from "./types";
export {WSConnection, WebsocketError, WebsocketService, SeashellWebsocket}
export * from "./storage";

enum OfflineMode {Off, On, Forced}

class Callback {
  constructor(public type: string, public cb: (message?: any) => any, public now: boolean) { }
}

class WebsocketService {
  private connection: WSConnection;
  // private synced: boolean;
  private connected: boolean;
  private failed: boolean;
  // private isSyncing: boolean;
  private offlineMode: number;
  private timeoutCount: number;
  private timeoutInterval: any;
  private key: number;

  private callbacks: Callback[];
  private socket: SeashellWebsocket;
  private storage: Storage;

  private compiler: SeashellCompiler;
  private runner: SeashellRunner;
  private tester: SeashellTester;

  public ping: ()=>Promise<WebsocketResult>;
  public newProject: (name: string)=>Promise<void>;
  public newProjectFrom: (name: string, src_url: string)=>Promise<WebsocketResult>;
  public deleteProject: (name: string)=>Promise<WebsocketResult>;
  public restoreFileFrom: (name: string, file: string, contents: string, history: History)=>Promise<WebsocketResult>;
  public getUploadFileToken: (name: string, file: string)=>Promise<WebsocketResult>;
  public getExportToken: (name: string)=>Promise<WebsocketResult>;
  public marmosetSubmit: (name: string, assn: string, subdir?: string)=>Promise<WebsocketResult>;

  public lockProject: (name: string)=>Promise<WebsocketResult>;
  public forceLockProject: (name: string)=>Promise<WebsocketResult>;
  public unlockProject: (name: string)=>Promise<WebsocketResult>;
  public archiveProjects: ()=>Promise<WebsocketResult>;

  public getProjects: ()=>Promise<Project[]>;
  public listProject: (name: string)=>Promise<WebsocketResult>;
  public readFile: (name: string, fileName: string)=>Promise<WebsocketResult>;
  public getFileToRun: (name: string, question: string)=>Promise<WebsocketResult>;
  public getSettings: ()=>Promise<WebsocketResult>;
  public getMostRecentlyUsed: (name: string, question: string)=>Promise<WebsocketResult>;

  public newFile: (name: string, fileName: string, contents: string, encoding: string, normalize: boolean)=>Promise<WebsocketResult>;
  public writeFile: (name: string, file: string, contents: string, history: History)=>Promise<WebsocketResult>;
  public deleteFile: (name: string, file: string)=>Promise<WebsocketResult>;
  public renameFile: (name: string, oldName: string, newName: string)=>Promise<WebsocketResult>;
  public setFileToRun: (name: string, question: string, folder: string, file: string)=>Promise<WebsocketResult>;
  public saveSettings: (settings: Settings)=>Promise<WebsocketResult>;
  public updateMostRecentlyUsed: (name: string, question: string, file: string)=>Promise<WebsocketResult>;

  constructor(connection: WSConnection, storage: Storage) {
    this.connection = connection;
    // this.synced = false;
    this.connected = false;
    this.failed = false;
    // this.isSyncing = false;
    this.offlineMode = 0; // TODO change this to look up the cookie
    this.timeoutCount = 0;
    this.timeoutInterval = null;
    this.key = 0;
    this.storage = storage;
    this.socket = new SeashellWebsocket(this.connection.wsURI, this.connection.publicKey);
    this.callbacks = [];

    // These functions are not available in offline mode.
    this.ping = this.make_offline_disabled('ping');
    this.newProject = this.make_offline_disabled('newProject');
    this.newProjectFrom = this.make_offline_disabled('newProjectFrom');
    this.deleteProject = this.make_offline_disabled('deleteProject');
    this.restoreFileFrom = this.make_offline_disabled('restoreFileFrom');
    this.getUploadFileToken = this.make_offline_disabled('getUploadFileToken');
    this.getExportToken = this.make_offline_disabled('getExportToken');
    this.marmosetSubmit = this.make_offline_disabled('marmosetSubmit');
    // These functions do nothing and just resolve in offline mode.
    this.lockProject = this.make_offline_noop('lockProject');
    this.forceLockProject = this.make_offline_noop('forceLockProject');
    this.unlockProject = this.make_offline_noop('unlockProject');
    this.archiveProjects = this.make_offline_noop('archiveProjects');
    
    // These functions either:
    //  - return the online result if online.
    //  - return the offline result if offline.
    this.getProjects = this.make_offline_enabled('getProjects');
    this.listProject = this.make_offline_enabled('listProject');
    this.readFile = this.make_offline_enabled('readFile');
    this.getFileToRun = this.make_offline_enabled('getFileToRun');
    this.getSettings = this.make_offline_enabled('getSettings');
    // this.getMostRecentlyUsed = this.make_offline_enabled('getMostRecentlyUsed');

    // These functions:
    //  - invoke the offline version if offline
    //  - invoke the both the offline version and the online
    //    version if online, returning the offline version.
    //  - marks the local storage as dirty and queues up a sync
    this.newFile = this.make_offline_enabled('newFile', true);
    this.writeFile = this.make_offline_enabled('writeFile', true);
    this.deleteFile = this.make_offline_enabled('deleteFile', true);
    this.renameFile = this.make_offline_enabled('renameFile', true);
    this.saveSettings = this.make_offline_enabled('saveSettings', true);
    this.setFileToRun = this.make_offline_enabled('setFileToRun', true);
    // this.updateMostRecentlyUsed = this.make_offline_enabled('updateMostRecentlyUsed', true)
  }

  // Connects the socket, sets up the disconnection monitor
  public async connect(): Promise<void> {
    /* TODO get cookie here
    if (! get(SEASHELL_CREDS_COOKIE)) {
      this.failed = true;
      this.invoke_cb('failed');
      throw error;
    }*/

    // Failure - probably want to prompt the user to attempt to reconnect or
    //  log in again
    this.socket.onFailure(async () => {
      this.failed = true;
      clearInterval(this.timeoutInterval);
      try {
        await this.invoke_cb_failure_om_wrap('failed'); 
      } catch (err) {
        console.error(err);
      }
    });

    // Socket closed - probably want to prompt the user to reconnect
    this.socket.onClose(async () => {
      this.connected = false;
      clearInterval(this.timeoutInterval);
      try {
        await this.invoke_cb_failure_om_wrap('disconnected'); 
      } catch (err) {
        console.error(err);
      }
    });

    try {
      await this.socket.authenticate();
    } catch (e) {
      this.failed = true;
      await this.invoke_cb_failure_om_wrap('failed');
      throw e;
    }
    
    console.log("Seashell socket set up properly");
    this.timeoutInterval = setInterval(async () => {
      try {
        if (this.timeoutCount++ === 3) {
          this.invoke_cb('timeout');
        }
        await this.socket.ping();
        if (this.timeoutCount >= 3) {
          this.invoke_cb('timein');
        }
        this.timeoutCount = 0;
      } catch (err) {
        console.error(err);
      }
    }, 4000);
    this.connected = true;
    this.failed = false;
    this.socket.requests[-3].callback = this.io_cb;
    this.socket.requests[-4].callback = this.test_cb;
    console.log("Websocket disconnection monitor set up properly.");
    // Run the callbacks.
    await this.invoke_cb('connected');
  }

  public disconnect(): void {
    this.socket.close();
  }

  public register_callback(type: string, cb: (message?: any) => any, now?: boolean) : number {
    this.callbacks[this.key] = new Callback(type, cb, now);

    if (type === 'disconnected' && !this.connected && now) {
      cb();
    } else if (type === 'connected' && (this.connected || this.isOffline()) && now) {
      cb();
    } else if (type === 'failed' && this.failed && now) {
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
      (x: Callback) => { return x && x.type === type; }).map(
        async (x: Callback) => { return x.cb(message); });
  }

  // wrapper function for invoke_cb meant to be used to call
  //  the websocket failure coniditons 'disconnected' and 'failure'
  public invoke_cb_failure_om_wrap(type: string, message?: any): PromiseLike<Array<any>> {
    if (this.offlineEnabled()) {
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

  public isConnected(): boolean {
    return this.connected;
  }

  public isOffline(): boolean {
    return this.offlineMode === OfflineMode.Forced || (!this.connected && this.offlineMode === OfflineMode.On);
  }

  public offlineEnabled(): boolean {
    return this.offlineMode === OfflineMode.On || this.offlineMode === OfflineMode.Forced;
  }

  public setOfflineModeSetting(setting: number): void {
    var old = this.offlineMode;
    if (setting === 0 || setting === 1 || setting === 2) {
      this.offlineMode = setting;
      // Set cookie, set expiry date to some date sufficiently in the future.
      var expiryDate = new Date();
      expiryDate.setFullYear(expiryDate.getFullYear() + 10);
      /* TODO set cookie here
      cookies.put(SEASHELL_OFFLINE_MODE_COOKIE, setting,
        {secure: true, expires: expiryDate});
      */
    } else {
      throw new WebsocketError("Invalid offline mode setting selected.", setting);
    }
    if (old === 2 && this.offlineMode !== old) {
      // trigger reconnect and sync
      if (!this.connected)
        this.connect();
      else {
        this.invoke_cb('connected');
        this.syncAll();
      }
    } else if (this.offlineMode === 2) {
      this.invoke_cb('connected', this.offlineMode);
    }
  }

  private make_offline_disabled(name: string) {
    return async (...args: any[]) => {
      if (!this.isOffline()) {
        var res = await this.socket[name].apply(this.socket, args);
        await this.syncAll();
        return res;
      } else {
        this.rejectOffline(name);
      }
    };
  }

  private make_offline_noop(name: string) {
    return async (...args: any[]) => {
      if (!this.isOffline()) {
        return this.socket[name].apply(this.socket, args);
      } else {
        return true;
      }
    };
  }

  private make_offline_enabled(name: string, offlineWriteThrough?: boolean) {
    const online_arity = this.socket[name].length;
    const offline_arity = this.storage[name].length;
    if (offline_arity != online_arity && offline_arity != online_arity+1) {
      throw new WebsocketError(`Offline and online arities differ for ${name}.`,
        {function: name, offline: offline_arity, online:online_arity});
    }
    if (online_arity != offline_arity) {
      console.log("Registering function %s which will use online result in write-through mode", name);
    }
    return async (...args: any[]) => {
      if (args.length > online_arity) {
        throw new WebsocketError(`Too many arguments passed to ${name}.`,
          {received: args.length, expected: online_arity});
      }
      if (this.offlineEnabled()) {
        if (! await this.storage.didInitSync()) {
          if (! this.isOffline()) {
            await this.syncAll();
          } else {
            throw new WebsocketError(`Cannot call ${name} in offline mode before an initial sync.`);
          }
        }
        return this.storage[name].apply(this.storage, args);
      } else {
        return this.socket[name].apply(this.socket, args);
      }
    };
  }

  public async compileAndRunProject(project: string, question: string, file: SeashellFile, tests: Array<Test>) {
    if (!this.isOffline()) {
      var test_names = tests.map((test: Test) => {
        return test.test_name;
      });
      return this.socket.compileAndRunProject(project, question, test_names);
    } else {
      if (file.ext() == 'rkt') {
        throw new WebsocketError("Racket files cannot be run in offline mode.");
      }
      var deps = await file.getDependencies(question);
      var file_arr = await deps.map(async (f: SeashellFile) => {
        return f.toWorker();
      });
      var result = await this.compiler.compile(project, question, file_arr, file.fullname());
      if (tests.length === 0) {
        // Fill in the PID with a fake, offline PID
        result.pid = await this.runner.run(result.obj, (message: Object, data: Object) => {
          this.io_cb(message, data);
        });
      } else {
        // Run the tests
        result.pids = await this.tester.runTests(result.obj, (message: Object, data: Object) => {
          this.test_cb(message, data);
        }, tests);
      }
      return result;
    }
  }

  public async programKill(pid: SeashellPID) {
    if (typeof pid === 'object') {
      pid.kill();
    } else {
      return this.socket.programKill(pid);
    }
  }

  public async sendEOF(pid: number|SeashellPID) {
    if (typeof pid === 'object') {
      pid.sendEOF();
    } else {
      return this.socket.sendEOF(pid);
    }
  }

  public async startIO(project: string, pid: number|SeashellPID) {
    if (typeof pid === 'object') {
      pid.startIO();
    } else {
      return this.socket.startIO(project, pid);
    }
  }

  /**
   * Sync everything, to be called when we first connect to the websocket.
   *
   * @returns Promise resolving to true when sync is done.
   */
  public async syncAll(): Promise<boolean> {
    if (!this.offlineEnabled()) {
      return false;
    }
    console.log("syncAll invoked");
    // this.isSyncing = true;
    var projects = await this.storage.getProjectsForSync();
    var files = await this.storage.listAllProjectsForSync();
    var changes = await this.storage.getOfflineChanges();
    var settings = await this.storage.getSettings(true);
    settings.values = JSON.stringify(settings.values);
    var result = await this.socket.sync({
      type: 'sync',
      projects: projects,
      files: files,
      changes: changes,
      settings: settings});
    await this.storage.applyChanges(result.changes, result.newProjects, result.deletedProjects, result.updatedProjects, result.settings);
    // this.isSyncing = false;
    return true;
  }

  private rejectOffline(name: string) {
    throw new WebsocketError(name + " is not available in offline mode.");
  }

  public hasOfflineChanges(): boolean {
    return this.storage.hasOfflineChanges();
  }
}
