export namespace DebugLogs {
  let enabled = false;
  // how many entries to keep in self.logs
  let logSize: number;
  let logs: {type: string, text: string}[] = [];
  // bug report email format
  const reportTo = "seashell@cs.uwaterloo.ca";
  const reportSubject = encodeURIComponent("Seashell issue report");
  function reportBody(): string {
    return encodeURIComponent("[Tell us what you were doing when this error showed up.]\n\n" + dump());
  };

  export function enable(size: number = 100): void {
    logSize = size;
    enabled = true;
  }
  export function disable(): void {
    enabled = false;
    logs = [];
  }
  // extend default js console.log
  export function overrideConsoleFn(fn: string): void {
    const win = window as any;
    const browserDefaultFn = win.console[fn];
    const stack = (new Error).stack as string;
    win.console[fn] = function(...args: any[]) {
      browserDefaultFn.apply(win.console, args);
      if (enabled) {
        logs.push({type: fn, text: prettyLogMsg.apply(this, arguments)});
        if (logs.length > logSize) {
            logs.shift();
        }
      }
    };
  }
  export function prettyLogMsg() {
    return Array.prototype.slice.call(arguments).map(function(arg: any) {
      try {
          // in case the object is recursive
          return JSON.stringify(arg);
      } catch (e) {
          return JSON.stringify(e);
      }
    }).join(" ");
  }
  // returns a human readable string of logs
  export function dump(): string {
    return "navigator.appVersion: " + navigator.appVersion + "\n\n" +
      logs.map(function(entry) {
          let prefix;
          switch (entry.type) {
              case "log": prefix   = "  log > "; break;
              case "warn": prefix  = " warn ! "; break;
              case "error": prefix = "error * "; break;
          }
          return prefix + entry.text;
      }).join("\n");
  };
  // display dump() in a new open window
  export function dumpNewWindow(): void {
    return window.open().document.write("<pre>" + dump() + "</pre>");
  };
  overrideConsoleFn("log");
  overrideConsoleFn("warn");
  overrideConsoleFn("error");
}

