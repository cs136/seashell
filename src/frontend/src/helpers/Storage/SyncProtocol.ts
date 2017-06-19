import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import * as E from "../Errors";
import Dexie from "dexie";
import "dexie-observable";
import "dexie-syncable";
export {SyncProtocol}

const RECONNECT_DELAY = 5000;

class SyncProtocol { // implements Dexie.Syncable.ISyncProtocol {

  constructor(private socket: SeashellWebsocket,
              public debug: boolean = false) { }

  public async sendChanges(changes: any, baseRevision: number, partial: boolean) {
    return this.socket.sendMessage({
      type: "changes",
      changes: changes,
      baseRevision: baseRevision,
      partial: partial
    });
  }

  public async clientIdentity(clientIdentity: string) {
    return this.socket.sendMessage({
      type: "clientIdentity",
      clientIdentity: clientIdentity
    });
  }

  public async subscribe(syncedRevision: number) {
    return this.socket.sendMessage({
      type: "subscribe",
      syncedRevision: syncedRevision
    });
  }

  public sync(context: any /*Dexie.Syncable.IPersistedContext*/, url: string, options: Object, baseRevision: any,
      syncedRevision: any, changes: any[] /*Dexie.Syncable.IDatabaseChange[]*/, partial: boolean,
      applyRemoteChanges: Function /*Dexie.Syncable.ApplyRemoteChangesFunction*/, onChangesAccepted: () => void,
      onSuccess: (continuation: any) => void, onError: (error: any, again?: number) => void) {

    this.clientIdentity(context.clientIdentity || null).then((res: any) => {
      context.clientIdentity = res.clientIdentity;
      context.save();
    });
    this.sendChanges(changes, baseRevision, partial).then(onChangesAccepted);
    this.subscribe(syncedRevision);

    this.socket.register_callback("failed", (message?: any) => {
      onError(message, RECONNECT_DELAY);
    });

    this.socket.register_callback("disconnected", (message?: any) => {
      onError("Socket closed: " + message, RECONNECT_DELAY);
    });

    let isFirstRound = true;
    this.socket.register_callback("changes", (request: any) => {
      const changes = /*<Dexie.Syncable.IDatabaseChange[]>*/request.changes;
      const currentRevision = <number>request.currentRevision;
      const partial = <boolean>request.partial;

      applyRemoteChanges(changes, currentRevision, partial);
      if (isFirstRound && !partial) {
        onSuccess({
          react: this.sendChanges.bind(this),
          disconnect: () => {}
        });
        isFirstRound = false;
      }
    });
  }
}
