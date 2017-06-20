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
      baseRevision: baseRevision ? baseRevision : 0,
      partial: partial
    });
  }

  public async clientIdentity(clientIdentity: string) {
    return this.socket.sendMessage({
      type: "clientIdentity",
      clientIdentity: clientIdentity ? clientIdentity : false
    });
  }

  public async subscribe(syncedRevision: number) {
    return this.socket.sendMessage({
      type: "subscribe",
      syncedRevision: syncedRevision ? syncedRevision : false
    });
  }

  public async sync(context: any /*Dexie.Syncable.IPersistedContext*/, url: string, options: Object, baseRevision: any,
      syncedRevision: any, changes: any[] /*Dexie.Syncable.IDatabaseChange[]*/, partial: boolean,
      applyRemoteChanges: Function /*Dexie.Syncable.ApplyRemoteChangesFunction*/, onChangesAccepted: () => void,
      onSuccess: (continuation: any) => void, onError: (error: any, again?: number) => void) {

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

    try {
      context.clientIdentity = await this.clientIdentity(context.clientIdentity);
      context.save();
      await this.sendChanges(changes, baseRevision, partial);
      onChangesAccepted();
      await this.subscribe(syncedRevision);
    } catch (e) {
      throw new E.WebsocketError("Error occurred while syncing.", e);
    }
  }
}
