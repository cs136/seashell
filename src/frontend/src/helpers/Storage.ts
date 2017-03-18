import {WebStorage} from "./Storage/WebStorage";
import {LocalStorage} from "./Storage/LocalStorage";
import {Connection} from "./Login";
import {AbstractStorage,
        File, FileID, FileBrief,
        Project, ProjectID, ProjectBrief,
        Settings, defaultSettings} from "./Storage/Interface";
export * from "./Storage/Interface";
export {Storage};


class Storage extends WebStorage implements AbstractStorage {
  private store: LocalStorage;
  constructor(cnn: Connection) {
    const name = `seashell-${cnn.username}`;
    const store = new LocalStorage(name);
    super(store);
  }
}
