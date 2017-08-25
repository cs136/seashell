import Dexie from "dexie";
import * as moment from "moment";

class Log {
  public type: string;
  public text: string;
  public time: string;
  public timems: number;
}

class LogDB extends Dexie {

  public logs: Dexie.Table<Log, number>;
  public limit: number = 100;

  constructor() {
    super("LogDB_3");
    this.version(1).stores({
      logs: "++, timems"
    });
    this.open();
  }

  public async pushLog(log: Log): Promise<void> {
    return this.transaction("rw", [this.logs], async () => {
      await this.logs.add(log);
      const count = await this.logs.count();
      if (count > this.limit) {
        await this.logs.orderBy("timems").reverse().offset(this.limit).delete();
      }
    });
  }

  public async clearLogs(): Promise<void> {
    return this.transaction("rw", [this.logs], async () => {
      await this.logs.clear();
    });
  }

  public async getAllLogs(): Promise<Log[]> {
    return this.transaction("rw", [this.logs], async () => {
      return this.logs.orderBy("timems").reverse().toArray();
    });
  }

  // returns a human readable string of logs
  public async dump(): Promise<string> {
    return "navigator.appVersion: " + navigator.appVersion + "\n\n" +
      (await this.getAllLogs()).map(function(entry) {
          let prefix;
          switch (entry.type) {
              case "log": return `<div>${entry.time} log > ${entry.text}</div>`;
              case "warn": return `<div style='font-color: yellow'>${entry.time} warn ! ${entry.text}</div>`;
              case "error": return `<div style='font-color: red'>${entry.time} error * ${entry.text}</div>`;
          }
      }).join("\n");
  };

  // display dump() in a new open window
  public async dumpNewWindow(): Promise<void> {
    const log = await this.dump();
    window.open().document.write("<div style='font-family: Monospace; white-space: nowrap;'>" + log + "</div>");
  };

}

export namespace DebugLogs {

  let enabled = false;
  let db = new LogDB();
  // bug report email format
  const reportTo = "seashell@cs.uwaterloo.ca";
  const reportSubject = encodeURIComponent("Seashell issue report");
  function reportBody(): string {
    return encodeURIComponent("[Tell us what you were doing when this error showed up.]\n\n" + this.db.dump());
  };

  export function enable(size: number = 100): void {
    enabled = true;
  }

  export function disable(): void {
    enabled = false;
  }

  function paramsToJson(): Promise<void> {
    return Array.prototype.slice.call(arguments).map(function(arg: any) {
      try {
          // in case the object is recursive
          return JSON.stringify(arg);
      } catch (e) {
          return JSON.stringify(e);
      }
    }).join(" ");
  }

  // extend default js console.log
  export function overrideConsoleFn(fn: string): void {
    const win = window as any;
    const browserDefaultFn = win.console[fn];
    const stack = (new Error).stack as string;
    win.console[fn] = function(...args: any[]) {
      browserDefaultFn.apply(win.console, args);
      if (enabled) {
        db.pushLog({
          type: fn,
          text: paramsToJson.apply(this, arguments),
          time: moment().format("MM/DD/HH:mm:ss"),
          timems: Date.now()
        });
      }
    };
  }

  overrideConsoleFn("log");
  overrideConsoleFn("warn");
  overrideConsoleFn("error");

  window.addEventListener("error", function (evt) {
      console.log("Caught[via 'error' event]:  '" + evt.message + "' from " + evt.filename + ":" + evt.lineno);
      console.log(evt); // has srcElement / target / etc
  });

  (window as any).logs = () => { db.dumpNewWindow(); };
}
