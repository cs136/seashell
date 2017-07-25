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

  private async connect(context: any, baseRevision: number, syncedRevision: number, changes: any, partial: boolean, onChangesAccepted: () => void): Promise<void> {
    context.clientIdentity = await this.clientIdentity(context.clientIdentity);
    context.save();
    await this.sendChanges(changes, baseRevision, partial);
    onChangesAccepted();
    await this.subscribe(syncedRevision);
  }

  public async sync(context: any /*Dexie.Syncable.IPersistedContext*/, url: string, options: Object, baseRevision: any,
      syncedRevision: any, changes: any[] /*Dexie.Syncable.IDatabaseChange[]*/, partial: boolean,
      applyRemoteChanges: Function /*Dexie.Syncable.ApplyRemoteChangesFunction*/, onChangesAccepted: () => void,
      onSuccess: (continuation: any) => void, onError: (error: any, again?: number) => void) {

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
    this.socket.register_callback("changes", (request: any) => {
      const changes = /*<Dexie.Syncable.IDatabaseChange[]>*/request.changes.map(deconvertChange);
      const currentRevision = <number>request.currentRevision;
      const partial = <boolean>request.partial;

      applyRemoteChanges(changes, currentRevision, partial);
      if (isFirstRound && !partial) {
        onSuccess({
          react: async (changes: any, baseRevision: number, partial: boolean, onChangesAccepted: () => void) => {
            await this.sendChanges(changes, baseRevision, partial);
            onChangesAccepted();
          },
          disconnect: () => {
            console.warn("disconnect called in sync continuation.");
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

    try {
      // reconnect the sync protocol after the websocket service has taken care
      //  of the automatic reconnecting.
      this.socket.register_callback("connected", (msg: any) => {
        onError(msg, 100);
      });

      await this.connect(context, baseRevision, syncedRevision, changes, partial, onChangesAccepted);
    } catch (e) {
      // don't throw errors that result from being disconnected
      if (this.socket.isConnected()) {
        throw new E.WebsocketError("Error occurred while syncing.", e);
      }
    }
  }
}