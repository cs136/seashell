import {SeashellWebsocket} from "../Websocket/WebsocketClient";
import {ChangeType} from "./Interface";
import {DispatchFunction} from "../Services";
import {appStateActions} from "../../reducers/appStateReducer";
import * as E from "../Errors";
import Dexie from "dexie";
import "dexie-observable";
import "dexie-syncable";

export {SyncProtocol}

class SyncProtocol { // implements Dexie.Syncable.ISyncProtocol {
  private change_key: number;
  private connect_key: number;

  constructor(private socket: SeashellWebsocket,
              private dispatch: DispatchFunction,
              public debug: boolean = false) { }

  private convertChange(change: any) {
    if (change.type === ChangeType.CREATE) {
      return {
        type: ChangeType.CREATE,
        table: change.table,
        key: change.key,
        data: JSON.stringify(change.obj)
      };
    } else if (change.type === ChangeType.UPDATE) {
      return {
        type: ChangeType.UPDATE,
        table: change.table,
        key: change.key,
        data: JSON.stringify(change.mods)
      };
    } else if (change.type === ChangeType.DELETE) {
      return {
        type: ChangeType.DELETE,
        table: change.table,
        key: change.key,
        data: ""
      };
    }
  };

  public async sendChanges(changes: any[], baseRevision: number, partial: boolean) {
    return this.socket.sendMessage({
      type: "changes",
      changes: changes.map(this.convertChange.bind(this)),
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
    console.log("Connecting sync protocol.");
    const deconvertChange = (change: any) => {
      if (change.type === ChangeType.CREATE) {
        let res = {
          type: ChangeType.CREATE,
          table: change.table,
          key: change.key,
          obj: JSON.parse(change.data)
        };
        res.obj.id = change.key;
        return res;
      } else if (change.type === ChangeType.UPDATE) {
        return {
          type: ChangeType.UPDATE,
          table: change.table,
          key: change.key,
          mods: JSON.parse(change.data)
        };
      } else if (change.type === ChangeType.DELETE) {
        return {
          type: ChangeType.DELETE,
          table: change.table,
          key: change.key
        };
      }
    };

    let isFirstRound = true;
    this.change_key = this.socket.register_callback("changes", async (request: any) => {
      const changes = /*<Dexie.Syncable.IDatabaseChange[]>*/request.changes.map(deconvertChange);
      const currentRevision = <number>request.currentRevision;
      const partial = <boolean>request.partial;

      await applyRemoteChanges(changes, currentRevision, partial);
      if (isFirstRound && !partial) {
        onSuccess({
          react: async (changes: any, baseRevision: number, partial: boolean, onChangesAccepted: () => void) => {
            await this.sendChanges(changes, baseRevision, partial);
            onChangesAccepted();
          },
          disconnect: () => {
            console.log("Disconnecting sync protocol due to request.");
            this.socket.unregister_callback(this.change_key);
            this.socket.unregister_callback(this.connect_key);
          }
        });
        isFirstRound = false;
      }
      if (changes.length > 0) {
        this.dispatch({
          type: appStateActions.applyServerChanges,
          payload: changes
        });
      }
    });

    // reconnect the sync protocol after the websocket service has taken care
    //  of the automatic reconnecting.
    this.connect_key = this.socket.register_callback("connected", (msg: any) => {
      onError(msg, 100);
    });


    try {
      context.clientIdentity = await this.clientIdentity(context.clientIdentity);
      context.save();
      await this.sendChanges(changes, baseRevision, partial);
      onChangesAccepted();
      await this.subscribe(syncedRevision);
    } catch (e) {
      // Couldn't connect -- if this is repairable, the connect callback will automatically
      // restart the protocol.
      onError(e, Infinity);
    }
  }
}
